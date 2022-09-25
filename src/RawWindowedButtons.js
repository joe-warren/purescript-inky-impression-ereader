//import { python } from 'pythonia'
const {python} = require('pythonia');

exports.runButtonsRaw = function(callback){
   async function go() {    
    const window = await python('./python_modules/windowed_buttons.py')
    const buttons = await window.buttons
    const makeCallback = function(i, isDown){
      return callback(i)(isDown)
    }
    for(i = 1; i <=4; i++){
      const btn = await buttons[i-1]
      await btn.bind('<ButtonPress>', makeCallback(i, true))
      await btn.bind('<ButtonRelease>', makeCallback(i, false))
    }

    await window.run()
    var done = false
    while(!done){
      done = await window.isDone();
    }

    python.exit() // Make sure to exit Python in the end to allow node to exit. You can also use process.exit.
  }
  return () => go();
}