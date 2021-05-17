var debug = false;
const debugLog = function(msg) {
    if(debug) console.log(msg);
};

const roomid     = location.pathname.split('/')[2];
const cookieName = `username-${roomid}`;
const roomCookie = document.cookie.split("; ")
      .find(cookie => cookie.startsWith(cookieName));
const username = roomCookie.split('=')[1];
document.getElementById('userdisplay').innerHTML = `Hello, ${username}!`;

let scheme = "wss";
let port   = 443;
if (location.protocol === 'http:') {
    scheme = "ws";
    port   = 80;
}
const ws = new WebSocket(`${scheme}://${location.host}${location.pathname}:${port}`);

ws.onclose = function(m) {
    console.log(`Connection close reason: ${m.reason}`);
    alert("This room is now closed");
    window.location = '/';
};

window.onbeforeunload = evt => {
    ws.close();
};

const LOCALSTACK = 'LOCAL';
const BROADSTACK = 'BROAD';
const protocol = function(action, extra = {}) {
    return (el) => {
        const stacktype = el.parentElement.id;
        const msg = Object.assign(extra, { action: action, stack: stacktype });
        debugLog(`From protocol: [${JSON.stringify(msg)}]`);
        ws.send(JSON.stringify(msg));
    };
};
const QUEUE_ACT     = 'QUEUE';
const NEXT_ACT      = 'NEXT';
const QOTHER_ACT    = 'QOTHER';
const ATTENDEES_ACT = 'UPDATE_ATTENDEES';
const UPDATE_WORLD  = 'WORLD';
const q      = protocol(QUEUE_ACT);
const next   = protocol(NEXT_ACT);
const qother = function(el) {
    const selID    = `${el.parentElement.id.toLowerCase()}-stack-other`;
    const attendee = document.getElementById(selID).value;
    protocol(QOTHER_ACT, {other: attendee})(el);
};

const appendQueue = function(stacktype, newItem) {
    const stackel = document.getElementById(`${stacktype}-discussion`);
    const newli   = document.createElement('li');
    newli.appendChild(document.createTextNode(newItem));
    stackel.appendChild(newli);
};

const popQueue = function(stacktype) {
    const stackel = document.getElementById(`${stacktype}-discussion`);
    stackel.removeChild(stackel.childNodes[0]);
};

const updateAttendees = function(attendees) {
    const attendeesel = document.getElementById('attendees');
    const selectl     = document.getElementById('local-stack-other');
    const selectb     = document.getElementById('broad-stack-other');
    attendeesel.innerHTML = '';
    selectl.innerHTML     = '';
    selectb.innerHTML     = '';
    attendees.forEach(attendee => {
        const li = document.createElement('li');
        li.appendChild(document.createTextNode(attendee));
        attendeesel.appendChild(li);
        // These fill in drop downs of only other attendees
        if (attendee !== username) {
            const localopt = document.createElement('option');
            localopt.value = attendee;
            localopt.appendChild(document.createTextNode(attendee));
            selectl.appendChild(localopt);
            const broadopt = document.createElement('option');
            broadopt.value = attendee;
            broadopt.appendChild(document.createTextNode(attendee));
            selectb.appendChild(broadopt);
        }
    });
};

const newQueue = function(stacktype, items) {
    const stackel = document.getElementById(`${stacktype.toLowerCase()}-discussion`);
    stackel.innerHTML = '';
    items.forEach(item => {
        const newli   = document.createElement('li');
        newli.appendChild(document.createTextNode(item));
        stackel.appendChild(newli);
    });
};

const setRoomName = function(name) {
    document.getElementById('roomheader').innerHTML = name;
};

const updateWorld = function(msg) {
    updateAttendees(msg['attendees']);
    newQueue(LOCALSTACK, msg['local']);
    newQueue(BROADSTACK, msg['broad']);
    setRoomName(msg['name']);
};

ws.onmessage = evt => {
    const m = JSON.parse(evt.data);
    debugLog(m);

    const payload = m['payload'];
    switch (m['action']) {
    case QUEUE_ACT:
        debugLog('Appending to stack');
        appendQueue(payload['stack'], payload['attendee']);
        break;
    case NEXT_ACT:
        debugLog('Popping stack');
        popQueue(payload['stack']);
        break;
    case ATTENDEES_ACT:
        debugLog('Updating attendees');
        updateAttendees(payload['attendees']);
        break;
    case UPDATE_WORLD:
        debugLog('Updating entire page');
        updateWorld(m);
        break;
    default:
        console.log(`Unknown action: [${evt.data}]`);
    }
};

ws.onopen = () => {
    const initMsg = { action: "ClientInit", room: roomid, attendee: username};
    ws.send(JSON.stringify(initMsg));
};

const deleteCookie = function() {
    document.cookie = `${cookieName}=NA; expires=Fri, 14 May 2021 00:00:00 UTC; path=/`;
};


const leaveRoom = function() {
    const leaveMsg = { action: "LEAVE" };
    ws.send(JSON.stringify(leaveMsg));
    ws.close();
    deleteCookie();
    window.location = `/?room=${roomid}`;
};

const closeRoom = function() {
    if(confirm("Are you sure? This will close the room for everyone!")) {
        const closeMsg = { action: "CLOSE" };
        ws.send(JSON.stringify(closeMsg));
        deleteCookie();
        window.location = '/';
    }
};
