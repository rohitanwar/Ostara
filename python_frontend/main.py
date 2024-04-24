import eel
import os
import json

@eel.expose
def save_data(data):
    with open('app/data.os', 'w') as f:
        f.write(data)
    print(os.popen('./Ostara').read())
    return 'done'

eel.init('web')
eel.start('main.html', size=(1000, 600), mode = None, port = 8080, host = 'localhost')


