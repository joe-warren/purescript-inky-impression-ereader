const {python, py} = require('pythonia');

const imagesModule =python("./python_modules/images.py")


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

exports.openAndResizeArbitraryImage = function(w){
    return function(h){
        return function(left){
            return function(right){
                return function(filename){
                    return async function go(){
                        let images = await python("./python_modules/images.py")
                        let img = await images.openAndResizeArbitrary(w, h, filename)
                        if (typeof img == 'string'){
                            return (left(img))
                        } else {
                            return right(img)
                        }
                    }
                }
            }
        }
    }
}

exports.size = function(img){
    return async function(){
        let images = await imagesModule
        let [w, h] = await images.size(img)
        let ww = await w
        let hh = await h
        return [ww, hh]
    }
}

exports.concatHRaw = function (a){
    return function(b){
        return async function(){
            let pil = await python("PIL")
            let [w1, h] = await exports.size(a)()
            let [w2,  ] = await exports.size(b)()
            const dst = await pil.Image.new('P',[w1+w2, h])
            const palette = await a.getpalette()
            await dst.putpalette(palette)
            await dst.paste(a, [0, 0])
            await dst.paste(b, [w1, 0])
            return dst
        }
    }
}


exports.concatVRaw = function (a){
    return function(b){
        return async function(){
            let pil = await python("PIL")
            let [w, h1] = await exports.size(a)()
            let [ , h2] = await exports.size(b)()
            const dst = await pil.Image.new('P',[w, h1+h2])
            const palette = await a.getpalette()
            await dst.putpalette(palette)
            await dst.paste(a, [0, 0])
            await dst.paste(b, [0, h1])
            return dst
        }
    }
}


exports.renderTextRaw = function(w){
    return function(h){
        return function (text){
            async function go(){
                let images = await python("./python_modules/images.py")
                let img = await images.renderText(w, h, text)
                return img
            }
            return go
        }
    }
}


exports.blankImageRaw = function(w){
    return function(h){
        return function (v){
            async function go(){
                let images = await python("./python_modules/images.py")
                let img = await images.blankImage(w, h, v)
                return img
            }
            return go
        }
    }
}


exports.rotateImageRaw = function(img){
    async function go(){
        // 4 = PIL.Image.Transpose.ROTATE_270
        return await img.transpose(4)
    }
    return go
}

