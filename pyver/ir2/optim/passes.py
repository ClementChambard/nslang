from dataclasses import dataclass
from ir2.data.prog import Function


@dataclass
class Pass:
    def run(self, prog) -> bool:
        return False


@dataclass
class FunctionPass(Pass):
    def run_on_function(self, f: Function) -> bool:
        return False

    def run(self, prog) -> bool:
        changed = False
        for d in prog:
            if isinstance(d, Function):
                changed |= self.run_on_function(d)
        return changed
