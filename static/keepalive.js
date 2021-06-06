var secondsTilReload = 10 * 60;
setTimeout(function() {
    location.reload();
}, secondsTilReload * 1000);

const clockEl = document.getElementById('clock');
const timer = function() {
    const min = Math.floor(secondsTilReload / 60);
    const sec = (secondsTilReload % 60).toLocaleString(undefined, {
        minimumIntegerDigits: 2, useGroupings: false
    });
    clockEl.innerHTML = `${min}:${sec}`;
    secondsTilReload--;
};
timer();
setInterval(timer, 1000);
