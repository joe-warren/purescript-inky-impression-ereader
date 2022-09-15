//import { python } from 'pythonia'
const {python} = require('pythonia');

exports.runButtonsRaw = function(callback){
   async function go() {    
    const tk = await python('tkinter')
    // All Python API access must be prefixed with await
    const root = await tk.Tk()
    await root.geometry('600x500')
    // A function call with a $ suffix will treat the last argument as a kwarg dict
    const makeCallback = function(i){
        return callback(i)
    }
    for(i = 1; i <=4; i++){
       const btn = await tk.Button$(root, {text: "" + i, command: makeCallback(i)} );
       await btn.grid({column: i, row: 0});
    }

    await root.mainloop()
    python.exit() // Make sure to exit Python in the end to allow node to exit. You can also use process.exit.
  }
  return () => go();
}