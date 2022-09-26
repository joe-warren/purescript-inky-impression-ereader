from PIL import Image

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
        img = Image.open(path).resize([w, h])

        pImg = Image.new('P', [w, h])
        pImg.putpalette(palette + [0, 0, 0]*(256-8))
        pImg.paste(img, (0, 0))
        return pImg
    except FileNotFoundError as e:
        return f"couldn't find ${path} (got ${e})"

def size(img):
    (w, h) = img.size
    return [w, h]
