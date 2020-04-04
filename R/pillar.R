
# Dynamically exported, see zzz.R
type_sum.phone <- function(x) "phone"

# Dynamically exported, see zzz.R
pillar_shaft.phone <- function(x, ...) {
  out <- format(x)
  out[is.na(x)] <- NA
  
  valid <- is_valid(x)
  
  out[!valid] <- pillar::style_neg(out[!valid])
  pillar::new_pillar_shaft_simple(out, align = "right")
}

# Dynamically exported, see zzz.R
is_vector_s3.phone <- function(x) TRUE

# Dynamically exported, see zzz.R
obj_sum.phone <- function(x) {
  paste0("phone [", length(x), "]")
}
