structure Reg where
  i : Nat

structure Loc where
  i: Nat

inductive Instr where
  | Z (dst: Reg)
  | S (srcdst: Reg)
  | T (src: Reg) (dst: Reg)
  | J (cmp0: Reg) (cmp1: Reg) (target: Loc)

def Prog := Array Instr

def Regs := Array Nat

instance : ToString Reg where
  toString r := s!"R{r.i + 1}"

instance : ToString Loc where
  toString l := s!"L{l.i + 1}"

instance : ToString Instr where
  toString i := match i with
    | Instr.Z d => s!"Z {d} := 1"
    | Instr.S s => s!"S {s}++"
    | Instr.T s d => s!"T {s} := {d}"
    | Instr.J c0 c1 t => s!"J if {c0} = {c1} goto {t}"

def iaction (i: Instr) (pc: Loc) (regs: Regs) :=
  let next := Loc.mk (pc.i + 1)
  match i with
  | Instr.Z dst => (next, regs.set! dst.i 0)
  | Instr.S srcdst => (next, regs.set! srcdst.i ((regs.get! srcdst.i) + 1))
  | Instr.T src dst => (next, regs.set! dst.i (regs.get! src.i))
  | Instr.J cmp0 cmp1 target =>
    if (regs.get! cmp0.i) = (regs.get! cmp1.i)
    then (target, regs) else (next, regs)

def execute_rec (prog: Prog) (pc: Loc) (regs: Regs) (fuel: Nat): Array Nat :=
  match fuel with
  | 0 => regs
  | Nat.succ n =>
    match prog.get? pc.i with
    | some i =>
      let (pc', regs') := (iaction i pc regs)
      -- dbg_trace regs'
      execute_rec prog pc' regs' n
    | none => regs

def load args (arr: Array Nat) (start: Nat) :=
  match args with
  | arg :: args' => load args' (arr.set! start arg) (start + 1)
  | [] => arr

def max_reg (prog: Prog) :=
  match (prog.map (λ i => match i with
    | Instr.Z dst => dst.i
    | Instr.S srcdst => srcdst.i
    | Instr.T src dst => Nat.max src.i dst.i
    | Instr.J cmp0 cmp1 _ => Nat.max cmp0.i cmp1.i)).getMax? (λ x y => x < y) with
  | some i => i
  | none => 0

def execute (fuel: Nat) (prog: Prog) (args: List Nat): Array Nat :=
  let regs := mkArray ((max_reg prog) + 1) 0
  let regs' := load args regs 0
  execute_rec prog (Loc.mk 0) regs' fuel

notation:10 "Z" k:11 ";" rest:10 => Array.insertAt rest 0 ((Instr.Z (Reg.mk (k - 1))))
notation:10 "S" k:11 ";" rest:10 => Array.insertAt rest 0 (Instr.S (Reg.mk (k - 1)))
notation:10 "T" k:11 "," m:11 ";" rest:10 => Array.insertAt rest 0 (Instr.T (Reg.mk (k - 1)) (Reg.mk (m - 1)))
notation:10 "J" k:11 "," l:11 "," m:11 ";" rest:10 => Array.insertAt rest 0 (Instr.J (Reg.mk (k - 1)) (Reg.mk (l - 1)) (Loc.mk (m - 1)))
notation:10 "HALT" => #[]

def execute_big := execute 100
