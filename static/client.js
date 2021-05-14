var debug = false;
const debugLog = function(msg) {
    if(debug) console.log(msg);
};

const roomid   = location.pathname.split('/')[2];
const roomCookie = document.cookie.split("; ")
      .find(cookie => cookie.startsWith(`username-${roomid}`));
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
const protocol = function(action) {
    return (el) => {
        const stacktype = el.parentElement.id;
        const msg = { action: action, stack: stacktype };
        debugLog(`From protocol: [${JSON.stringify(msg)}]`);
        ws.send(JSON.stringify(msg));
    };
};
const QUEUE_ACT     = 'QUEUE';
const NEXT_ACT      = 'NEXT';
const ATTENDEES_ACT = 'UPDATE_ATTENDEES';
const UPDATE_WORLD  = 'WORLD';
const q    = protocol(QUEUE_ACT);
const next = protocol(NEXT_ACT);

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
    attendeesel.innerHTML = '';
    attendees.forEach(attendee => {
        const li = document.createElement('li');
        li.appendChild(document.createTextNode(attendee));
        attendeesel.appendChild(li);
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


const leaveRoom = function() {
    const leaveMsg = { action: "LEAVE" };
    ws.send(JSON.stringify(leaveMsg));
    ws.close();
    window.location = `/?room=${roomid}`;
};

const closeRoom = function() {
    if(confirm("Are you sure? This will close the room for everyone!")) {
        const closeMsg = { action: "CLOSE" };
        ws.send(JSON.stringify(closeMsg));
        window.location = '/';
    }
};
