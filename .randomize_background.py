import os
import glob
import subprocess
import time
import random

images = glob.glob('wallpaper/*')

first = images[0]

delay = 5

while True:
    while first == images[0]:
        random.shuffle(images)

    for image in images:
        feh = subprocess.Popen(['feh', '--bg-scale', image])
        time.sleep(delay * 60)

        first = image

        feh.terminate()
    random.shuffle(images)
