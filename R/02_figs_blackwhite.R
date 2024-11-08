
# this file nees 
# 1_manuscript where data are read, labels are introduced etc.
# AND 01_read_process_data


# fun: draw mainresults CI ------------------------------------------------------

fig_mainresults_part <- function(dat_in, ymax_in = NA, n_breaks_x = 10, show_errorbar = TRUE, ylab_in = "est", xlab_in = "x"){
  dat_in %>% copy() %>%  
    .[,sukup := factor(sukup, levels = labs_lst$sukup$val, labels = labs_lst$sukup$lab)] %>% 
    .[, sample := factor(sample, levels = labs_lst$sample$val, labels = labs_lst$sample$lab)] %>% 
    ggplot(aes(y = est, x = x, linetype = factor(sample)))+
    #geom_point()+
    {if(show_errorbar) geom_ribbon(aes(ymin =lower, ymax=upper), color = NA, alpha = .6, fill = "gray80", show.legend=FALSE)}+
    #    {if(show_errorbar) geom_errorbar(aes(ymin =lower, ymax=upper), color = "black", width = .5)}+
    geom_line(aes(linetype = factor(sample)))+
    labs(y = ylab_in, x = xlab_in, linetype = "")+
    scale_x_continuous(n.breaks = n_breaks_x)+
    scale_y_continuous(limits = c(0, ymax_in), expand = c(0,0))
    #scale_color_jama()+
    #scale_fill_jama(guide = "none")
}



# fig-mainresults --------------------------------------------------------------

fig_mr <- list()

## parts to list ----

fig_mr$a <- fig_mainresults_part(fig_dat_mr$ci[dg == "first" & sukup == 2, head(.SD, 1), .(x = floor(x),sample)], 
                          ymax_in = 80, 
                          ylab_in = labs_lst$ci_title, 
                          xlab_in = labs_lst$age_title)+ 
  labs(title = paste0("A ", labs_lst$ci, ", ", labs_lst$sukup[val == 2, lab %>% firstlower()]))

fig_mr$b <- fig_mainresults_part(fig_dat_mr$ci[dg == "first" & sukup == 1, head(.SD, 1), .(x = floor(x),sample)], 
                          ymax_in = 80, 
                          ylab_in = labs_lst$ci_title, 
                          xlab_in = labs_lst$age_title)+ 
  labs(title = paste0("B ", labs_lst$ci, ", ", labs_lst$sukup[val == 1, lab %>% firstlower()]))

fig_mr$c <- fig_mainresults_part(fig_dat_mr$ir[dg == "first" & sukup == 2], 
                          ymax_in = 850, 
                          ylab_in = labs_lst$ir_title, 
                          xlab_in = labs_lst$age_title)+ 
  labs(title = paste0("C ", labs_lst$ir, ", ", labs_lst$sukup[val == 2, lab %>% firstlower()]))

fig_mr$d <- fig_mainresults_part(fig_dat_mr$ir[dg == "first" & sukup == 1], 
                          ymax_in = 850, 
                          ylab_in = labs_lst$ir_title, 
                          xlab_in = labs_lst$age_title)+ 
  labs(title = paste0("D ", labs_lst$ir, ", ", labs_lst$sukup[val == 1, lab %>% firstlower()]))

fig_mr$e <- fig_mainresults_part(fig_dat_mr$pr[sukup == 2 & year == pr_year & dg == "first"], 
                          ymax_in = 20, 
                          ylab_in = labs_lst$pr_title, 
                          xlab_in = labs_lst$age_title, 
                          show_errorbar = F)+ 
  labs(title = paste0("E ", labs_lst$pr, ", ", labs_lst$sukup[val == 2, lab %>% firstlower()]))

fig_mr$f <- fig_mainresults_part(fig_dat_mr$pr[sukup == 1 & year == pr_year & dg == "first"], 
                          ymax_in = 20, ylab_in = labs_lst$pr_title, 
                          xlab_in = labs_lst$age_title, show_errorbar = F)+ 
  labs(title = paste0("F ", labs_lst$pr, ", ", labs_lst$sukup[val == 1, lab %>% firstlower()]))

## fig ----
# kasataan manuscritpissa

(fig_mr$a+fig_mr$b) /(fig_mr$c+fig_mr$d) / (fig_mr$e+fig_mr$f) +  plot_layout(guides = 'collect') & theme(legend.position = 'bottom')


# Fig dgs ----------------------------------------------------------------------

fig_dgs <- list()

## fun: draw dgs CI ------------------------------------------------------

# eli tehdaan diagnoosipaaryhmittain kuvat, jossa
# color sukup,
# vasemmalla ci, oikealla pr

fig_dgs_part <- function(dat_in, dg_in, sukup_in, part_in, ymax_in = NA, show_errorbar = TRUE, 
                         ylab_in = labs_lst$ci_title, xlab_in = labs_lst$age_title){
  dat_in[dg == dg_in & sample == "all", head(.SD, 1), .(x = floor(x),sukup)] %>% copy() %>%  
    .[,sukup := factor(sukup, levels = labs_lst$sukup$val, labels = labs_lst$sukup$lab)] %>% 
    .[, sample := factor(sample, levels = labs_lst$sample$val, labels = labs_lst$sample$lab)] %>% 
    ggplot(aes(y = est, x = x, linetype = factor(sukup)))+
    #geom_point()+
#    {if(show_errorbar) geom_errorbar(aes(ymin =lower, ymax=upper), color = "gray", width = .5)}+
    {if(show_errorbar) geom_ribbon(aes(ymin =lower, ymax=upper), alpha = .4, fill = "gray80", color = NA, show.legend=FALSE)}+
    labs(y = ylab_in, x = xlab_in, linetype = "", title = paste0(part_in), subtitle = dgs_tbl[dg == dg_in, labs_short])+
    geom_line()+
    ylim(c(0, ymax_in))+
    xlim(c(0, 100))+
    scale_color_jama()+
    scale_fill_jama(guide = "none")+
    theme(plot.subtitle = element_textbox_simple(halign = .05))
}

#yleisyys, taman mukaan jarjestykseen
fig_dat_mr$ci[sample == "all" & time == 99][, max(est), dg][order(-V1)]

draw_fig_dgs <- function(dg1 = "f4" , dg2 = "f3" , dg3 = "f0", dg4 =  "f5", ymaxci = 80, ymaxir = 800, ymaxpr = 20){
  a_ci <- fig_dgs_part(fig_dat_mr$ci, dg_in = dg1, sukup_in = 2, part_in = paste0("A ", labs_lst$ci), ymax_in = ymaxci)
  a_ir <- fig_dgs_part(fig_dat_mr$ir, dg_in = dg1, sukup_in = 2, part_in = paste0("B ", labs_lst$ir), ymax_in = ymaxir, ylab_in = labs_lst$ir_title)
  a_pr <- fig_dgs_part(fig_dat_mr$pr[year == 2019 & x >0], dg_in = dg1, sukup_in = 2, part_in = paste0("C ", "Service utilization"), ymax_in = ymaxpr, 
                       show_errorbar = F, ylab_in = labs_lst$pr_title)
  
  b_ci <- fig_dgs_part(fig_dat_mr$ci, dg_in = dg2, sukup_in = 2, part_in = "", ymax_in = ymaxci)
  b_ir <- fig_dgs_part(fig_dat_mr$ir, dg_in = dg2, sukup_in = 2, part_in = "", ymax_in = ymaxir, ylab_in = labs_lst$ir_title)
  b_pr <- fig_dgs_part(fig_dat_mr$pr[year == 2019 & x >0], dg_in = dg2, sukup_in = 2, part_in = "", ymax_in = ymaxpr, 
                       show_errorbar = F, ylab_in = labs_lst$pr_title)
  
  c_ci <- fig_dgs_part(fig_dat_mr$ci, dg_in = dg3, sukup_in = 2, part_in = "", ymax_in = ymaxci)
  c_ir <- fig_dgs_part(fig_dat_mr$ir, dg_in = dg3, sukup_in = 2, part_in = "", ymax_in = ymaxir, ylab_in = labs_lst$ir_title)
  c_pr <- fig_dgs_part(fig_dat_mr$pr[year == 2019 & x >0], dg_in = dg3, sukup_in = 2, part_in = "", ymax_in = ymaxpr, 
                       show_errorbar = F, ylab_in = labs_lst$pr_title)
  
  d_ci <- fig_dgs_part(fig_dat_mr$ci, dg_in = dg4, sukup_in = 2, part_in = "", ymax_in = ymaxci)
  d_ir <- fig_dgs_part(fig_dat_mr$ir, dg_in = dg4, sukup_in = 2, part_in = "", ymax_in = ymaxir, ylab_in = labs_lst$ir_title)
  d_pr <- fig_dgs_part(fig_dat_mr$pr[year == 2019 & x >0], dg_in = dg4, sukup_in = 2, part_in = "", ymax_in = ymaxpr, 
                       show_errorbar = F, ylab_in = labs_lst$pr_title)
  
  fig <-  
    a_ci + b_ci + c_ci + d_ci +
    a_ir+  b_ir + c_ir + d_ir +
    a_pr + b_pr + c_pr +  d_pr +  
    plot_layout(guides = 'collect', ncol = 4) & theme(legend.position = 'bottom')
  
  fig
  
}

fig_dgs$part1 <- draw_fig_dgs(dg1 = "first" , dg2 = "first_no_f0" , dg3 = "f4", dg4 =  "f3", ymaxci = 80, ymaxir = 800, ymaxpr = 20)
fig_dgs$part1

## PART 2
fig_dgs$part2 <- draw_fig_dgs(dg1 = "f0", dg2 = "f5", dg3 = "f1", dg4 = "f9", ymaxci = 40, ymaxir = 800, ymaxpr = 15)
fig_dgs$part2

## PART 3
fig_dgs$part3 <- draw_fig_dgs(dg1 = "f8", dg2 = "f6", dg3 = "f2", dg4 = "f7", ymaxci = 15, ymaxir = 300, ymaxpr = 10)
fig_dgs$part3

# fig_dgs$ymaxci <-10
# fig_dgs$ymaxpr <- 2.5
# fig_dgs$a <- fig_dgs_part(fig_dat_mr$ci, dg_in = "f2", sukup_in = 2, part_in = "A", ymax_in = fig_dgs$ymaxci)
# fig_dgs$b <- fig_dgs_part(fig_dat_mr$pr[year == 2019], dg_in = "f2", sukup_in = 2, part_in = "B", ymax_in = fig_dgs$ymaxpr, 
#                           show_errorbar = F, ylab_in = labs_lst$pr_title)
# 
# fig_dgs$c <- fig_dgs_part(fig_dat_mr$ci, dg_in = "f7", sukup_in = 2, part_in = "C", ymax_in = fig_dgs$ymaxci)
# fig_dgs$d <- fig_dgs_part(fig_dat_mr$pr[year == 2019], dg_in = "f7", sukup_in = 2, part_in = "D", ymax_in = fig_dgs$ymaxpr, 
#                           show_errorbar = F, ylab_in = labs_lst$pr_title)
# 
# fig_dgs$e <- fig_dgs_part(fig_dat_mr$ci, dg_in = "psychoses", sukup_in = 2, part_in = "E", ymax_in = fig_dgs$ymaxci)
# fig_dgs$f <- fig_dgs_part(fig_dat_mr$pr[year == 2019], dg_in = "psychoses", sukup_in = 2, part_in = "F", ymax_in = fig_dgs$ymaxpr, 
#                           show_errorbar = F, ylab_in = labs_lst$pr_title)
# 
# fig_dgs$g <- fig_dgs_part(fig_dat_mr$ci, dg_in = "imtm", sukup_in = 2, part_in = "G", ymax_in = fig_dgs$ymaxci)
# fig_dgs$h <- fig_dgs_part(fig_dat_mr$pr[year == 2019], dg_in = "imtm", sukup_in = 2, part_in = "H", ymax_in = fig_dgs$ymaxpr, 
#                           show_errorbar = F, ylab_in = labs_lst$pr_title)
# 
# fig_dgs$part3 <- fig_dgs$a + fig_dgs$b+ fig_dgs$c+ fig_dgs$d + fig_dgs$e + fig_dgs$f + fig_dgs$g + fig_dgs$h + 
#   plot_layout(guides = 'collect', ncol = 2) & theme(legend.position = 'bottom')

# Supplement -------------------------------------------------------------------

# fig CI compare dgs ----------------------------------------------------------

draw_compare_dgs_part <- function(dat_in, sukup_in = 1, ymax_in = 50, xmax_in = 135, ylab_in = "est", xlab_in = "x"){
  d <- dat_in[dg %in% dgs_tbl[sub_chapter==T, dg] & sukup == sukup_in & sample == "all"]
  tex <- d[x == max(x)]
  
  tex <- merge(
    tex,
    dgs_tbl[, .(dg, labs =labs %>% word(start = -1) %>% sub("\\(", "", .) %>% sub("\\)", "", .))],
    by = "dg",
    all.x = T
  )
  
  tex[, dg := labs]
  
  d %>%
    .[, dg := factor(dg)] %>% 
    ggplot(aes(y = est, x = x, group = dg, color = dg))+
    geom_line()+
    geom_ribbon(aes(ymin =lower, ymax=upper, fill = dg), alpha = .4, color = NA)+
    geom_label_repel(data = tex, aes(label = dg),nudge_x = 5, direction = "y", hjust = "right", max.overlaps = 10,
                     min.segment.length = 0, color = "black", xlim = c(100,NA),
                     arrow = arrow(length = unit(0.015, "npc")))+
    theme(legend.position = "none")+
    scale_y_continuous(limits = c(0, ymax_in), expand = c(0,0))+
    scale_x_continuous(limits = c(0, xmax_in), breaks  = seq(0,100, 25))+
    scale_color_d3()+
    labs(y = ylab_in, x = xlab_in, color = "")
}

fig_compare_dgs <- list()

fig_compare_dgs$ci <- (draw_compare_dgs_part(fig_dat_mr$ci, sukup_in = 2, ylab_in = labs_lst$ci_title, xlab_in = labs_lst$age_title) + 
                         labs(title = paste("A", labs_lst$sukup[val == 2, lab]))) + 
  (draw_compare_dgs_part(fig_dat_mr$ci, sukup_in = 1, ylab_in = labs_lst$ci_title, xlab_in = labs_lst$age_title) + 
     labs(title = paste("B", labs_lst$sukup[val == 1, lab]))) + plot_layout(guides = "collect")

fig_compare_dgs$ir <- (draw_compare_dgs_part(fig_dat_mr$ir, sukup_in = 2, ymax_in = 800, ylab_in = labs_lst$ir_title, xlab_in = labs_lst$age_title) + 
                         labs(title = paste("A", labs_lst$sukup[val == 2, lab]))) + 
  (draw_compare_dgs_part(fig_dat_mr$ir, sukup_in = 1, ymax_in = 800, ylab_in = labs_lst$ir_title, xlab_in = labs_lst$age_title) + 
     labs(title = paste("B", labs_lst$sukup[val == 1, lab])))




# fig-mainresultsdg

# draw_fig_mrdg <- function(dg_in){
#   
#   ceiling_max <- function(x){
#     ifelse(x >= 10, ifelse( x>= 100, ceiling(x / 100)*100, ceiling(x / 10)*10) , ifelse(x <1, ceiling(x * 10)/10, x %>% ceiling()))
#   }
#   
#   ym <- fig_dat_mr$ci[dg == dg_in, max(upper) %>% ceiling_max()]
#   fig_mr$a <- fig_mainresults_part(fig_dat_mr$ci[dg == dg_in & sukup == 2], 
#                                    ymax_in = ym, 
#                                    ylab_in = labs_lst$ci_title, 
#                                    xlab_in = labs_lst$age_title)+ 
#     labs(title = paste("A", labs_lst$sukup[val == 2, lab]))
#   
#   fig_mr$b <- fig_mainresults_part(fig_dat_mr$ci[dg == dg_in & sukup == 1], 
#                                    ymax_in = ym, 
#                                    ylab_in = labs_lst$ci_title, 
#                                    xlab_in = labs_lst$age_title)+ 
#     labs(title = paste("B", labs_lst$sukup[val == 1, lab]))
#   
#   ym <- fig_dat_mr$ir[dg == dg_in, max(upper, na.rm = T) %>% ceiling_max()]
#   fig_mr$c <- fig_mainresults_part(fig_dat_mr$ir[dg == dg_in & sukup == 2], 
#                                    ymax_in = ym, 
#                                    ylab_in = labs_lst$ir_title, 
#                                    xlab_in = labs_lst$agegr_title)+ 
#     labs(title = paste("C", labs_lst$sukup[val == 2, lab]))
#   
#   fig_mr$d <- fig_mainresults_part(fig_dat_mr$ir[dg == dg_in & sukup == 1], 
#                                    ymax_in = ym, 
#                                    n_breaks_x = NULL,
#                                    ylab_in = labs_lst$ir_title, 
#                                    xlab_in = labs_lst$agegr_title)+ 
#     labs(title = paste("D", labs_lst$sukup[val == 1, lab]))
#   
#   ym <- fig_dat_mr$pr[dg == dg_in, max(est, na.rm = T) %>% ceiling_max()]
#   fig_mr$e <- fig_mainresults_part(fig_dat_mr$pr[sukup == 2 & year == pr_year & dg == dg_in], 
#                                    ymax_in = ym, 
#                                    ylab_in = labs_lst$pr_title, 
#                                    xlab_in = labs_lst$agegr_title, 
#                                    show_errorbar = F)+ 
#     labs(title = paste("E", labs_lst$sukup[val == 2, lab]))
#   
#   fig_mr$f <- fig_mainresults_part(fig_dat_mr$pr[sukup == 1 & year == pr_year & dg == dg_in], 
#                                    ymax_in = ym, 
#                                    ylab_in = labs_lst$pr_title, 
#                                    xlab_in = labs_lst$agegr_title, show_errorbar = F)+ 
#     labs(title = paste("F", labs_lst$sukup[val == 1, lab]))
#   
#   (fig_mr$a+fig_mr$b) /(fig_mr$c+fig_mr$d) / (fig_mr$e+fig_mr$f) +  plot_layout(guides = 'collect') & theme(legend.position = 'bottom')
#   
#   }

# RIPSA tassa cohort kuvat:

## suppfig cohort ----

suppfig_cohort <- datlist_incidence$ci_results_cohort %>% rbindlist(idcol = "dg", fill = T) %>% .[, dg := dg %>% tolower()] %>%
  #.[dg == "f4" & time<25] %>%
  .[, dg := factor(dg, levels = paste0("f", seq(1,9)), labels = dgs_tbl[dg %in% paste0("f", seq(1,9)), labs_short])] %>% 
  .[,sukup := factor(sukup, levels = labs_lst$sukup$val, labels = labs_lst$sukup$lab)] %>% 
  ggplot(aes(y =est, x = time, group = factor(group), colour = factor(group)))+
  geom_line()+
  facet_rep_grid(sukup~dg, repeat.tick.labels = T, labeller = label_wrap_gen(multi_line = TRUE))+
  theme(legend.position = "none",  strip.background = element_blank())
suppfig_cohort

## suppfig cohort95 ----

suppfig_cohort95 <- list(
  c95 = datlist_incidence$ci_results_cohort_95_99 %>% rbindlist(idcol = "dg", fill = T) %>% .[, dg := dg %>% tolower()],
  full = datlist_incidence$ci_results_all_specialities  %>% .[, dg := dg %>% tolower()]
) %>% rbindlist(idcol = "cohort") %>%
  .[, dg := dg %>% tolower()] %>% 
#  .[group %in% c("all_time", cohorts) & time <=25] %>% 
  .[dg %in% paste0("f", seq(1,9)) & time <=25] %>%
  .[, dg := factor(dg, levels = paste0("f", seq(1,9)), labels = dgs_tbl[dg %in% paste0("f", seq(1,9)), labs_short])] %>% 
  .[,sukup := factor(sukup, levels = labs_lst$sukup$val, labels = labs_lst$sukup$lab)] %>% 
    .[, cohort := factor(cohort, levels = c("full", "c95"), labels = c("Pooled estimates", "Birth cohorts with complete follow-up"))] %>% 
  ggplot(aes(y =est, x = time, group = factor(cohort), colour = factor(cohort), fill = factor(cohort)))+
  geom_line()+
  geom_ribbon(aes(ymin =lower, ymax=upper), color = NA, alpha = .6)+
  facet_rep_grid(sukup~dg, repeat.tick.labels = T, labeller = label_wrap_gen(multi_line = TRUE))+
  labs(y = labs_lst$ci_title, x = labs_lst$age_title, color = "")+
  scale_fill_jama(guide = "none")+
  scale_color_jama()+
  theme(legend.position = 'bottom', strip.background = element_blank())

suppfig_cohort95
