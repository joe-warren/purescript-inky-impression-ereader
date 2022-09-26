import RPi.GPIO as GPIO
import threading as threading
import queue
buttons = [5, 6, 16, 24]

GPIO.setmode(GPIO.BCM)

GPIO.setup(buttons, GPIO.IN, pull_up_down=GPIO.PUD_UP)
q = queue.Queue(maxsize=100);

def cb(pin):
    q.put([buttons.index(pin)+1, GPIO.input(pin) == 0])

for pin in buttons:
    GPIO.add_event_detect(pin, GPIO.BOTH, cb)

def next():
    try:
        return q.get(block=False)
    except queue.Empty:
        return None