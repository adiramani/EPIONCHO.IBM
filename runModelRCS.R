library(dplyr)

iter <- as.numeric(Sys.getenv("PBS_ARRAY_INDEX"))
set.seed(iter + (iter*3758))

DT.in <- 1/366
timesteps = 100
give.treat.in = 0
treat.strt = 1; treat.stp = 16
trt.int = 1

ABR.in <- 1450

output_equilibrium <-  ep.equi.sim(time.its = timesteps,
                                   ABR = ABR.in,
                                   treat.int = trt.int,
                                   treat.prob = 0.80,
                                   give.treat = give.treat.in,
                                   treat.start = treat.strt,
                                   treat.stop = treat.stp,
                                   treat.timing = NA,
                                   pnc = 0.01,
                                   min.mont.age = 5,
                                   vector.control.strt = NA,
                                   delta.hz.in = 0.186,
                                   delta.hinf.in = 0.003,
                                   c.h.in = 0.005,
                                   gam.dis.in = 0.3,
                                   run_equilibrium=TRUE,
                                   print_progress=TRUE,
                                   calc_ov16=TRUE)

treat.len = 18; treat.strt.yrs = 100; yrs.post.treat = 0

treat.strt = treat.strt.yrs; treat.stp = treat.strt + treat.len
timesteps = treat.stp + yrs.post.treat #final duration

give.treat.in = 1; trt.int = 1

output <- ep.equi.sim(time.its = timesteps,
                      ABR = ABR.in,
                      treat.int = trt.int,
                      treat.prob = 0.80,
                      give.treat = give.treat.in,
                      treat.start = treat.strt,
                      treat.stop = treat.stp,
                      treat.timing = NA,
                      pnc = 0.01,
                      min.mont.age = 5,
                      vector.control.strt = NA,
                      delta.hz.in = 0.186,
                      delta.hinf.in = 0.003,
                      c.h.in = 0.005,
                      gam.dis.in = 0.3,
                      run_equilibrium = FALSE,
                      equilibrium = output_equilibrium$all_equilibrium_outputs,
                      print_progress=TRUE,
                      calc_ov16 = TRUE,
                      ov16_equilibrium = output_equilibrium$ov16_equilibrium)

saveRDS(output, paste("/rds/general/user/ar722/home/ov16_test/ov16_output/ov16_any_worm_output",iter,".rds", sep=""))

