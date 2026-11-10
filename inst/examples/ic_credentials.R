# a virtual chunk container: where chunk bytes may come from
ic_container("https://noaa-cdr-sea-surface-temp-optimum-interpolation-pds.s3.amazonaws.com/", "http")
ic_container("s3://my-bucket/data/", "s3", region = "us-west-2")
ic_container("file:///data/netcdf/", "local", path = "/data/netcdf")
# credentials for storage
ic_cred_s3("anonymous")
\dontrun{
ic_cred_s3("static", access_key_id = "AKIA...", secret_access_key = "...")
}
