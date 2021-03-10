def pipeline(stages, payload):
    for x in stages:
        x(payload)

    return payload
