const {python} = require('pythonia');

exports.runButtonsRaw = function(callback){
   async function go() {    
    const gpio = await python('RPi.GPIO')
    const signal = await python('signal')
    const pins = [5, 6, 16, 24]

    const bcm = await gpio.BCM
    await gpio.setmode(bcm)
    const iin = await gpio.IN;
    const pud_up = await gpio.PUD_UP
    await gpio.setup$(pins, iin, {pull_up_down: pud_up });
    const makeCallback = function(i, isDown){
        return callback(i)(isDown)
    }
    const both = await gpio.BOTH
    const falling = await gpio.FALLING
    
    const cb = function (pin, edge) {
       callback(pins.indexOf(pin) + 1)(edge == falling)()
    }

    for(i = 1; i <=4; i++){
       await gpio.add_event_detect(pins[i-1], both, cb);
    }
    //await (new Promise (x => {}))
    //python.exit() // Make sure to exit Python in the end to allow node to exit. You can also use process.exit.
  }
  return () => go();
}