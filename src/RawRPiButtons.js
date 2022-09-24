const {python} = require('pythonia');

exports.runButtonsRaw = function(callback){
   async function go() {    
    const buttons = await python('./buttons.py')
    while(true){
        const r = await buttons.next();
        if(r){
          const [pi, pdn] = r;
          const i = await pi;
          const dn = await pdn;
          callback(i)(dn)();
        }
    }
    //await (new Promise (x => {}))
    //python.exit() // Make sure to exit Python in the end to allow node to exit. You can also use process.exit.
  }
  return () => go();
}