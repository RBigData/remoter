get_ips = function()
{
  ip_in = tryCatch(getip::getip("internal"), error=identity)
  if (inherits(tryCatch(ip_in, error=identity), "error"))
    ip_in  = "ERROR: couldn't determine internal IP"
  
  ip_ex = tryCatch(getip::getip("external"), error=identity)
  if (inherits(tryCatch(ip_ex, error=identity), "error"))
    ip_ex  = "ERROR: couldn't determine external IP"
  
  return(list(ip_in=ip_in, ip_ex=ip_ex))
}
