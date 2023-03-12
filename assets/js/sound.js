var volume = 1

function loadSound(file, name) {
    createjs.Sound.registerSound("assets/sounds/" + file, name);
}

function playSound(name) {
    createjs.Sound.play(name, { volume: 0.125 * volume })
}

function setVolume(amount) {
    createjs.Sound.volume = 0.125 * amount
    volume = amount
}