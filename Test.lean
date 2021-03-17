-- Using List as carrier of `children`: works
partial def recurseM [Monad μ] (curr: α) (action: α -> μ (List α)) : μ PUnit := do
  let children ← action curr
  children.forM (recurseM · action)

#reduce recurseM () (fun _ => some []) -- some 
def specificTraverseList : Option Unit := recurseM (μ:=Option) () (fun _ => [])


-- Using Array as carrier of `children`:
partial def recurseM2 [Monad μ] (curr: α) (action: α -> μ (Array α)) : μ PUnit := do
  let children ← action curr
  children.forM (recurseM2 · action)

#reduce recurseM2 (μ:=Option) () (fun _ => #[])
-- When compiling definition below:
-- `deep recursion was detected at 'replace' (potential solution: increase stack space in your system)`
def specificTraverseArray : Option Unit := recurseM2 () (fun _ => some #[])