//import { python } from 'pythonia'
const {python} = require('pythonia');

exports.runButtonsRaw = function(callback){
   async function go() {    
    const tk = await python('tkinter')
    const root = await tk.Tk()
    await root.geometry('600x500')
    const makeCallback = function(i, isDown){
        return callback(i)(isDown)
    }
    for(i = 1; i <=4; i++){
       const btn = await tk.Button$(root, {text: "" + i});
       await btn.bind('<ButtonPress>', makeCallback(i, true))
       await btn.bind('<ButtonRelease>', makeCallback(i, false))
       await btn.grid({column: 0, row: i});
    }

    await root.mainloop()
    python.exit() // Make sure to exit Python in the end to allow node to exit. You can also use process.exit.
  }
  return () => go();
}