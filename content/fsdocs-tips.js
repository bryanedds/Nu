let currentTip = null;
let currentTipElement = null;

function hideTip(evt, name, unique) {
    const el = document.getElementById(name);
    el.style.display = "none";
    currentTip = null;
}

function hideUsingEsc(e) {
    hideTip(e, currentTipElement, currentTip);
}

function showTip(evt, name, unique, owner) {
    document.onkeydown = hideUsingEsc;
    if (currentTip === unique) return;
    currentTip = unique;
    currentTipElement = name;

    const offset = 20;
    let x = evt.clientX;
    let y = evt.clientY + offset;

    const el = document.getElementById(name);
    el.style.position = "absolute";
    el.style.display = "block";
    el.style.left = `${x}px`;
    el.style.top = `${y}px`;
    const maxWidth = document.documentElement.clientWidth - x - 16;
    el.style.maxWidth = `${maxWidth}px`;

    const rect =  el.getBoundingClientRect();
    // Move tooltip if it is out of sight
    if(rect.bottom > window.innerHeight) {
        y = y - el.clientHeight - offset;
        el.style.top = `${y}px`;
    }

    if (rect.right > window.innerWidth) {
        x = y - el.clientWidth - offset;
        el.style.left = `${x}px`;
        const maxWidth = document.documentElement.clientWidth - x - 16;
        el.style.maxWidth = `${maxWidth}px`;
    }
}

function Clipboard_CopyTo(value) {
    const tempInput = document.createElement("input");
    tempInput.value = value;
    document.body.appendChild(tempInput);
    tempInput.select();
    document.execCommand("copy");
    document.body.removeChild(tempInput);
}

window.showTip = showTip;
window.hideTip = hideTip;
// Used by API documentation
window.Clipboard_CopyTo = Clipboard_CopyTo;