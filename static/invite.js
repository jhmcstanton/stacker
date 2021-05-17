const verifyform = function(form) {
    let valid = true;
    Array.from(form.elements).forEach(el => {
        if (el.type === 'text' && !el.value) {
            alert(`${el.name} must be provided`);
            valid = false;
        }
    });
    return valid;
};

const copier = function(){
    const shareurl = document.getElementById('copylink').innerHTML;
    const el = document.createElement('textarea');
    el.value = shareurl;
    document.body.appendChild(el);
    el.select();
    el.setSelectionRange(0, 99999);
    document.execCommand('copy');
    document.body.removeChild(el);
    alert('Copied the link!');
};

const maybeRoomID = new URLSearchParams(location.search).get('room');

if (maybeRoomID) {
    document.getElementById('room-id').value = maybeRoomID;
}
if (typeof roomid !== 'undefined') {
    const shareurl = `http://${location.host}?room=${roomid}`;
    document.getElementById('copylink').innerHTML = shareurl;
};
