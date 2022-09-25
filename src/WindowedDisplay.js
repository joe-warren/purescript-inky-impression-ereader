const {python} = require('pythonia');

exports.windowedDisplayRaw = function(img) {
    return async function go(){
        let window = await python("./python_modules/windowed_canvas.py")
        await window.display(img)
    }
}