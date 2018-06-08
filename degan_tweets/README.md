---
title: Setting up a scheduled tweet using R on EC2
output: 
  html_document: 
    theme: cosmo
    toc: yes
---


### BLUF
This guides you through setting up a scheduled tweet-bot library on AWS EC2, and posting using R. 

### Summary
I usedI generally try to write ever-green/non-ephemeral content pieces for a few reasons. First, it focuses


### Setting up EC2
Any kind of Linux AMI. 

```bash
sudo yum -y install R
```

I encountered some issues installing devtools, but followed the instructions from [here](https://stackoverflow.com/questions/20923209/problems-installing-the-devtools-package) and got it working. I just needed to:

```bash
sudo yum -y install libcurl libcurl-devel
sudo yum -y install openssl-devel
chmod 7503
```

### The *R* code  
Yes, I know you can do this with Python, Ruby or whatever. I know R best and this was the quickest way to get it up and running. Don't hate. 

#### 
###Making a library

### Scheduling: setting up cron jobs
- I had some issues editing the cron file, or more specifically _having my edits save_. You need to do this weird thing. 