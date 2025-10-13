from ir2.data.instrs.memory import LoadInstr, LocalVarInstr, StoreInstr


def is_alloca_promotable(ai: LocalVarInstr) -> bool:
    users_set = set()
    users = []
    for u in ai.uses:
        if id(u.user) not in users_set:
            users_set.add(id(u.user))
            users.append(u.user)

    for u in users:
        if isinstance(u, LoadInstr):
            if u.ty != ai.ty:
                return False
        elif isinstance(u, StoreInstr):
            if u.val is ai or u.val.ty != ai.ty:
                return False
        # TODO: more instructions
        else:
            return False

    return True
