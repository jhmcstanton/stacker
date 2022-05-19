var debug = false;
const debugLog = function(msg) {
    if(debug) console.log(msg);
};

const roomid     = location.pathname.split('/')[2];
const cookieName = `username-${roomid}`;
const roomCookie = document.cookie.split("; ")
      .find(cookie => cookie.startsWith(cookieName));
var username = roomCookie.split('=')[1];
const attendees   = {};
const localStack  = [];
const globalStack = [];

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
        const stacktype = typeof el === 'string' ? el : el.parentElement.parentElement.id;
        const msg = Object.assign(extra, { action: action, stack: stacktype });
        debugLog(`From protocol: [${JSON.stringify(msg)}, el: [${el.parentElement.id}]`);
        ws.send(JSON.stringify(msg));
    };
};
const QUEUE_ACT     = 'QUEUE';
const NEXT_ACT      = 'NEXT';
const QOTHER_ACT    = 'QOTHER';
const ATTENDEES_ACT = 'UPDATE_ATTENDEES';
const UPDATE_WORLD  = 'WORLD';
const REORDER_ACT   = 'REORDER';
const CANCEL_ACT    = 'CANCEL';
const ADD_ACT       = 'ADD';
const q      = protocol(QUEUE_ACT);
const next   = protocol(NEXT_ACT);
const qother = function(el) {
    const selID    = `${el.parentElement.parentElement.id.toLowerCase()}-stack-other`;
    const attendee = document.getElementById(selID).value;
    protocol(QOTHER_ACT, {other: attendee})(el);
};

const shiftUp = function(i, stacktype) {
    shifter(i, i - 1, stacktype);
};

const shiftDown = function(i, stacktype) {
    shifter(i, i + 1, stacktype);
};

const swap = function(xs, l, r) {
    if (l < 0 || l >= xs.length || r < 0 || r >= xs.length) return xs;
    const holdthis = xs[l];
    xs[l] = xs[r];
    xs[r] = holdthis;
    return xs;
};

const shifter = function(l, r, stacktype) {
    const stack = [...stacktype === LOCALSTACK ? localStack : globalStack];
    swap(stack, l, r);
    protocol(REORDER_ACT, { newstack: stack })(stacktype);
};

const cancel = function(index, stacktype) {
    const stack = [...stacktype === LOCALSTACK ? localStack : globalStack];
    stack.splice(index, 1);
    protocol(CANCEL_ACT, { newstack: stack })(stacktype);
};

const addOtherAttendee = function() {
    const otherEl = document.getElementById("other-attendee-username");
    const other   = otherEl.value.trim();
    if (!other) {
        alert("Attendee name is required!");
        return;
    }
    const msg = { action: ADD_ACT, other: other };
    debugLog(`From protocol: [${JSON.stringify(msg)}]`);
    ws.send(JSON.stringify(msg));
    otherEl.value = '';
};

const updateAttendees = function(attendees) {
    const attendeesel = document.getElementById('attendees');
    const selectl     = document.getElementById('local-stack-other');
    const selectb     = document.getElementById('broad-stack-other');
    attendeesel.innerHTML = '';
    selectl.innerHTML     = '';
    selectb.innerHTML     = '';
    for(const [attendee, count] of Object.entries(attendees)) {
        const row = document.createElement('tr');
        const attendeeEl = document.createElement('td');
        attendeeEl.appendChild(document.createTextNode(attendee));
        row.appendChild(attendeeEl);
        const countEl = document.createElement('td');
        countEl.appendChild(document.createTextNode(count));
        row.appendChild(countEl);
        attendeesel.appendChild(row);
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
    };
};

const newQueue = function(stacktype, items) {
    const stackel = document.getElementById(`${stacktype.toLowerCase()}-discussion`);
    stackel.innerHTML = '';
    let i = 0;
    items.forEach(item => {
        let k = i;
        const newli   = document.createElement('li');
        newli.setAttribute('class', 'grid grid-cols-2');
        const spanNode = document.createElement('span');
        spanNode.setAttribute('class', 'inline-flex items-center');
        newli.appendChild(spanNode);
        spanNode.appendChild(document.createTextNode(item));
        const buttonWrapper = document.createElement('div');
        buttonWrapper.setAttribute('class', 'justify-self-end justify-items-end inline');
        newli.appendChild(buttonWrapper);
        const upButton = document.createElement('button');
        upButton.appendChild(document.createTextNode('↑'));
        upButton.onclick = function() { shiftUp(k, stacktype); };
        const downButton = document.createElement('button');
        downButton.appendChild(document.createTextNode('↓'));
        downButton.onclick = function() { shiftDown(k, stacktype); };

        const cancelButton = document.createElement('button');
        cancelButton.appendChild(document.createTextNode("✕"));
        cancelButton.onclick = function() { cancel(k, stacktype); };

        buttonWrapper.appendChild(upButton);
        buttonWrapper.appendChild(downButton);
        buttonWrapper.appendChild(cancelButton);
        stackel.appendChild(newli);
        i++;
    });
};

const setRoomName = function(name) {
    document.getElementById('roomheader').innerHTML = name;
};

const copyArray = function(xs, ys) {
    xs.length = 0;
    ys.forEach(x => xs.push(x));
};

const copyObj = function(xs, ys) {
    Object.keys(ys).forEach(k => {
        xs[k] = ys[k];
    });
    Object.keys(xs).forEach(k => {
        if (!ys.hasOwnProperty(k)) {
            delete xs[k];
        }
    });
};

const updateWorld = function(msg) {
    copyObj(attendees, msg['attendeeToCount']);
    copyArray(localStack, msg['local']);
    copyArray(globalStack, msg['broad']);
    updateAttendees(attendees);
    newQueue(LOCALSTACK, localStack);
    newQueue(BROADSTACK, globalStack);
    setRoomName(msg['name']);
};

const setUserDisplay = function(username) {
    document.getElementById('userdisplay').innerHTML = `Hello, ${username}!`;
};

const initRoom = function() {
    const initMsg = { action: "CLIENTINIT", room: roomid, attendee: username};
    debugLog(`Initializing with ${JSON.stringify(initMsg)}`);
    ws.send(JSON.stringify(initMsg));
};

const retryUser = function() {
    debugLog('Retrying user');
    username = '';
    while (username === '') {
        username = prompt("Your username is taken, please provide a new name");
    }
    initRoom();
    const exDate = new Date();
    exDate.setDate(exDate.getDate() + 1);
    document.cookie = `${cookieName}=${username}; ${exDate.toUTCString()}; path=/; SameSite=Strict`;
    setUserDisplay(username);
};

ws.onmessage = evt => {
    const m = JSON.parse(evt.data);
    debugLog(m);

    switch (m['action']) {
    case "DUPLICATEUSER":
        debugLog('Duplicate user');
        updateAttendees(m['attendees']);
        retryUser();
        break;
    case UPDATE_WORLD:
        debugLog('Updating entire page');
        updateWorld(m);
        break;
    default:
        console.log(`Unknown action: [${evt.data}]`);
    }
};

ws.onopen = initRoom;

const deleteCookie = function() {
    document.cookie = `${cookieName}=NA; expires=Fri, 14 May 2021 00:00:00 UTC; path=/; SameSite=Strict`;
};


const leaveRoom = function() {
    const leaveMsg = { action: "LEAVE" };
    ws.send(JSON.stringify(leaveMsg));
    ws.close();
    deleteCookie();
    window.location = `/?room=${roomid}`;
};

const closeRoom = function() {
    if(hash('facilitator') && confirm("Are you sure? This will close the room for everyone!")) {
        const closeMsg = { action: "CLOSE" };
        ws.send(JSON.stringify(closeMsg));
        deleteCookie();
        window.location = '/';
    }
};
const hash = function(k) {
    return document.location.hash.includes(k);
};
const revealElementByClassName = (frag, name) => function () {
    if (hash(frag)) {
        for (el of document.getElementsByClassName(name)) {
            el.classList.remove(name);
        }
    }
};
const setFacilitatorScreen = revealElementByClassName('facilitator', 'controls');
const setStack2            = revealElementByClassName('stacktwo', 'stacktw');

setUserDisplay(username);
setFacilitatorScreen();
setStack2();
