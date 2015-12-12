(* L6 Compiler TypeChecker
 * Ben Plaut, William Tong
 *
 * Description to follow
 *)

(* prints error message and raises ErrorMsg.error if error found *)
val typecheck : TypeInfAst.untypedPostElabOverallAST -> TypeInfAst.typedPostElabAST
