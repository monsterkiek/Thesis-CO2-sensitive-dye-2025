from bpdefs import WAIT, LOG, SETCONTROL, Nothing, CheckBox, Text, Button, DropDown, RadioBtns, EditBox, DataDict

steps=[
# Wait for a time duration: WAIT(dur="float" [,units='Seconds' (Seconds|Minutes|Hours)])
WAIT(dur="7",units="Hours"),
# Close a log file: LOG(close='')
LOG(close=0),
# Set a control: SETCONTROL('target', 'value', 'eval' [,opt_target=''])
SETCONTROL("PowerState","Sleep",""),
]
