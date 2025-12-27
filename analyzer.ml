let run program =
  let program' = Name_resolution.run program in
  Type_check.check program';
  Return_validation.run program'
