const {python, py} = require('pythonia');

exports.openPalettized = function(left){
    return function(right){
        return function (filename){
            async function go(){
                let images = await python("./python_modules/images.py")
                let img = await images.openPalettized(filename)
                if (typeof img == 'string'){
                    return (left(img))
                } else {
                    return right(img)
                }
            }
            return go
        }
    }
}

exports.size = function(img){
    return async function(){
        let images = await python("./python_modules/images.py")
        return await images.size(img)
    }
}