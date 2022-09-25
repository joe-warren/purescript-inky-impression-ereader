import tkinter as tk
from PIL import ImageTk
import threading 
import queue

root = tk.Tk()
root.geometry('640x449')
root.grid()
root.title("EReader Dev Preview")
buttons = [tk.Button(root, text=str(i+1), pady=40) for i in range(4)]

for i, btn in enumerate(buttons):
    btn.grid(column= 1, row=i+1)

canvas = tk.Canvas(root, height=448, width=600)
canvas.grid(column=2, row=1, rowspan=4, columnspan=1, sticky='ns')


# 🤮 if this is garbage collected, the rendering breaks
tk_canvas_image = None

def display(img):
    global tk_canvas_image
    tk_canvas_image = ImageTk.PhotoImage(img.convert('RGB'), master=canvas)
    canvas.create_image(0, 0,image=tk_canvas_image, anchor='nw')

q = queue.Queue(maxsize=1);

def run():
    pass

def isDone():
    root.update_idletasks()
    root.update()
    return False
