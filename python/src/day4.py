from typing import List

from dateutil import parser as date_parser

class Record():
    def __init__(self, time):
        self.time = time

class BeginShiftRecord(Record):
    def __init__(self, time, guard):
        super().__init__(time)
        self.guard = guard

class FallAsleepRecord(Record):
    pass

class WakeUpRecord(Record):
    pass



def solve():
    pass

def parse(lines: List[str]) -> List[Record]:
    for l in lines:
        date_str = l.split('[')[1].split(']')[0]
        


def run(fname: str) -> None:
    with open(fname, 'r') as f:
        solve(parse(f.readlines()))
