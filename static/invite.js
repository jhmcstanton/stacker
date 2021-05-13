const maybeRoomID = new URLSearchParams(location.search).get('room');

if (maybeRoomID) {
    document.getElementById('room-id').value = maybeRoomID;
}
if (roomid) {
    const shareurl = `http://${location.host}?room=${roomid}`;
    document.getElementById('copylink').innerHTML = shareurl;
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
