"""
Protocol to take an reflectance measurement using the orange LED (590nm)
very 2 min for 56 times (repeats).
"""

_protocol = [
    {
        "share": 1,
        "_protocol_set_": [
            {
                "label": "Absorbance 590nm",
                "pulses": [
                    1
                ],
                "pulse_distance": [
                    1205
                ],
                "pulsed_lights_brightness": [
                    [
                        15000  # max -2000, withou minus works in micrimol of light
                    ]
                ],
                "pulse_length": [
                    [
                        150
                    ]
                ],
                "pulsed_lights": [
                    [
                        3
                    ]
                ],
                "detectors": [
                    [
                        3
                    ]
                ],
                "measurements": 10,
                "measurements_delay": 1205  # 120500 ms - 1000 us for pulse distance check if needed
            }
        ]
    }
]

# max number out put of 50000


def _analyze(data):

    out = {}

    repeats = _protocol[0]['_protocol_set_'][0]['measurements']
    delay = _protocol[0]['_protocol_set_'][0]['measurements_delay']

    out['Time [min]'] = []

    for i in range(1, repeats+1):
        out['Time [min]'].append((i * delay) / 1000 / 60)

    out['Absorbance [590 nm]'] = []
    out['Reflectance [590 nm]'] = []
    out['Reflectance [655 nm]'] = []


    for p in data['set']:
        out['Absorbance [590 nm]'].append(
            p['data_raw'][0])  # add first and only element
        out['Reflectance [590 nm]'].append(
            p['data_raw'][0])  # add first and only element
        out['Reflectance [655 nm]'].append(
            p['data_raw'][0])  # add first and only element


    # Now calculate absorbance as I/I0
    out['Absorbance [590 nm]'] = [x/out['Absorbance [590 nm]'][0]
                                  for x in out['Absorbance [590 nm]']]

    return out
