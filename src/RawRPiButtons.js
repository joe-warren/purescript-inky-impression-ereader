const {python} = require('pythonia');

exports.runButtonsRaw = function(callback){
   async function go() {    
    const gpio = await python('gpiozero')
    const signal = await python('signal')
    const pins = [5, 6, 16, 24]


    const makeCallback = function(i, isDown){
        return callback(i)(isDown)
    }
    for(i = 1; i <=4; i++){
       const btn = await gpio.Button(pins[i-1]);
       btn.when_pressed = makeCallback(i, true);
       btn.when_released = makeCallback(i, false);
    }
    await signal.pause()
    python.exit() // Make sure to exit Python in the end to allow node to exit. You can also use process.exit.
  }
  return () => go();
}