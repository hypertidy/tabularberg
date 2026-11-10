st <- ic_storage_local(file.path(tempdir(), "a-repo"))
st
ic_storage_memory()
ic_storage_http("https://data.source.coop/ausantarctic/ausantarctic-scratch/oisst/oisst-v2.1-daily/icechunk")
\dontrun{
# cloud object stores (the "cloud" build tier)
ic_storage_s3("my-bucket", prefix = "repo", region = "us-west-2",
              credentials = ic_cred_s3("from_env"))
ic_storage_s3("bucket", prefix = "repo", endpoint_url = "https://projects.pawsey.org.au",
              anonymous = TRUE)
ic_storage_gcs("my-bucket", prefix = "repo", anonymous = TRUE)
ic_storage_azure("account", "container", prefix = "repo")
}
