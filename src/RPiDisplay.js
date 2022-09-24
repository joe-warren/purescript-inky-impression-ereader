const {python} = require('pythonia');


exports.rpiDisplayRaw = function(img) {
    return async function go(){
        let display = await python("./python_modules/rpi_display.py")
        await display.setImage(img)
    }
}