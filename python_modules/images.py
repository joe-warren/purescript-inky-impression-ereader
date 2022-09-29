from PIL import Image, ImageDraw, ImageFont

palette = [
    0, 0, 0,
    255, 255, 255,
    0, 255, 0,
    0, 0, 255,
    255, 0, 0,
    255, 255, 0,
    255, 140, 0,
    255, 255, 255
    ]

def openPalettized(path):
    try:
        img = Image.open(path)
        if img.mode != 'P':
            return f"{path} is not palettized"
        p = img.getpalette()
        if p[0:len(palette)] != palette:
           return f"{path} has the wrong palette (got ${p})"
        return img
    except FileNotFoundError as e:
        return f"couldn't find ${path} (got ${e})"

def openAndResizeArbitrary(w, h, path):
    try:
        orig = Image.open(path)
        wF, hF = orig.size
        desiredSize = [int(w), int(w*hF/wF)]
        if desiredSize[1] > h:
            desiredSize = [int(wF*h/hF), int(h)]

        img = orig.resize(desiredSize).convert('RGB')

        pImg = Image.new('RGB', [w, h], color=(255,255,255))
        pImg.paste(img, (int((w-desiredSize[0])/2), int((h-desiredSize[1])/2)))
        palimage = Image.new('P', (16, 16))
        palimage.putpalette(palette + [0, 0, 0]*(256-8))
        newimage = pImg.quantize(palette=palimage)
        return newimage
    except FileNotFoundError as e:
        return f"couldn't find ${path} (got ${e})"

def size(img):
    (w, h) = img.size
    return [w, h]

def renderText(w, h, text):
    img = Image.new('P', (w, h), color=1)
    img.putpalette(palette + [0, 0, 0]*(256-8))
    font = ImageFont.truetype("assets/SourceSansPro-Regular.ttf", 12)
    d_usr = ImageDraw.Draw(img)
    d_usr.text((int(w/2),int(h/2)), text ,0, font=font, anchor="mm")
    return img


def blankImage(w, h, v):
    img = Image.new('P', (w, h), color=int(v))
    img.putpalette(palette + [0, 0, 0]*(256-8))
    return img