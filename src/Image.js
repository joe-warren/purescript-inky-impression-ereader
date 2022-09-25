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

exports.size = function(img){
    return async function(){
        let images = await imagesModule
        return await images.size(img)
    }
}

exports.concatHRaw = function (a){
    return function(b){
        return async function(){
            let pil = await python("PIL")
            let [w1p, hp] = await exports.size(a)()
            let [w2p, ] = await exports.size(b)()
            let w1 = await w1p;
            let w2 = await w2p;
            let h = await hp;
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
            let [wp, h1p] = await exports.size(a)()
            let [_, h2p] = await exports.size(b)()
            let w = await wp
            let h1 = await h1p
            let h2 = await h2p
            const dst = await pil.Image.new('P',[w, h1+h2])
            const palette = await a.getpalette()
            await dst.putpalette(palette)
            await dst.paste(a, [0, 0])
            await dst.paste(b, [0, h1])
            return dst
        }
    }
}