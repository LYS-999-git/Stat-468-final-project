# Use official R base image
FROM rocker/r-ver:4.4.0

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev libssl-dev libxml2-dev \
    libgit2-dev libsodium-dev curl unzip \
    && rm -rf /var/lib/apt/lists/*

# Install renv globally
RUN R -e "install.packages('renv')"

# Copy project files
COPY api.R /app/
COPY renv.lock /app/
WORKDIR /app

# Restore packages from renv.lock
RUN R -e "renv::restore(lockfile = 'renv.lock')"

# Expose API port
EXPOSE 8000

# Setting AWS credentials to Dockerfile
ENV AWS_ACCESS_KEY_ID=AKIA3IMG4BYSJMKLAJMU
ENV AWS_SECRET_ACCESS_KEY=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
ENV AWS_DEFAULT_REGION=us-east-1
ENV S3_BUCKET=stat-468-model

# Run the Vetiver API
CMD ["Rscript", "api.R"]
