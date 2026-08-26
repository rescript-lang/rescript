@val @scope("Math") @module("m") external f: int => int = "f"
let a = import(f)
