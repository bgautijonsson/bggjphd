library(analogsea)
library(bggjphd)
library(tidyverse)
library(future)

d <- docklet_create()


d |> docklet_pull("r-base")

d |> droplet_ssh("docker ps", verbose = TRUE)
d |> droplet_ssh("docker exec 35e737e40df4 Rscript -e 'bggjphd::stations'", verbose = T)


ip <- d |> droplet_ip()

ssh_private_key_file <- "/Users/brynjolfurjonsson/.ssh/id_rsa"

# Connect and create a cluster
cl <- makeClusterPSOCK(
  ip,

  # User name; DigitalOcean droplets use root by default
  user = "root",

  # Use private SSH key registered with DigitalOcean
  rshopts = c(
    "-o", "StrictHostKeyChecking=no",
    "-o", "IdentitiesOnly=yes",
    "-i", ssh_private_key_file
  ),

  # Command to run on each remote machine
  # The script loads the tidyverse Docker image
  # --net=host allows it to communicate back to this computer
  rscript = c("sudo", "docker", "run", "--net=host",
              "rocker/tidyverse", "Rscript"),

  # These are additional commands that are run on the remote machine.
  # At minimum, the remote machine needs the future library to work—installing furrr also installs future.
  rscript_args = c(
    # Create directory for package installation
    "-e", shQuote("local({p <- Sys.getenv('R_LIBS_USER'); dir.create(p, recursive = TRUE, showWarnings = FALSE); .libPaths(p)})"),
    # Install furrr and future
    "-e", shQuote("if (!requireNamespace('furrr', quietly = TRUE)) install.packages('furrr')"),
    # Install pak for package management
    "-e", shQuote("if (!requireNamespace('remotes', quietly = TRUE)) install.packages('remotes')"),
    # Install PHD Package
    "-e", shQuote("if (!requireNamespace('bggjphd', quietly = TRUE)) remotes::install_github('bgautijonsson/bggjphd')")
  ),

  # Actually run this stuff. Set to TRUE if you don't want it to run remotely.
  dryrun = FALSE
)


plan(cluster, workers = cl)

n_cpus %<-% {print(1)}
n_cpus

plan(sequential)

print(1)


