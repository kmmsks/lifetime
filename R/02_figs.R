
# this file needs 01_read_process_data.R to be run first


# fun: draw mainresults CI ------------------------------------------------------

fig_mainresults_part <- function(dat_in, ymax_in = NA, n_breaks_x = 10, show_errorbar = TRUE, ylab_in = "est", xlab_in = "x"){
  dat_in %>% copy() %>%  
    .[,sukup := factor(sukup, levels = labs_lst$sukup$val, labels = labs_lst$sukup$lab)] %>% 
    .[, sample := factor(sample, levels = labs_lst$sample$val, labels = labs_lst$sample$lab)] %>% 
    ggplot(aes(y = est, x = x, color = factor(sample), fill = factor(sample)))+
    #geom_point()+
    geom_line()+
    {if(show_errorbar) geom_ribbon(aes(ymin =lower, ymax=upper), color = NA, alpha = .6)}+
    #    {if(show_errorbar) geom_errorbar(aes(ymin =lower, ymax=upper), color = "black", width = .5)}+
    labs(y = ylab_in, x = xlab_in, color = "")+
    scale_x_continuous(n.breaks = n_breaks_x)+
    scale_y_continuous(limits = c(0, ymax_in), expand = c(0,0))+
    scale_color_jama()+
    scale_fill_jama(guide = "none")
}



# fig-mainresults --------------------------------------------------------------

fig_mr <- list()

## parts to list ----

fig_mr$a <- fig_mainresults_part(fig_dat_mr$ci[dg == "first" & sukup == 2], 
                                 ymax_in = 80, 
                                 ylab_in = labs_lst$ci_title, 
                                 xlab_in = labs_lst$age_title)+ 
  labs(title = paste0("a ", labs_lst$ci, ", ", labs_lst$sukup[val == 2, lab %>% firstlower()]))

fig_mr$b <- fig_mainresults_part(fig_dat_mr$ci[dg == "first" & sukup == 1], 
                                 ymax_in = 80, 
                                 ylab_in = labs_lst$ci_title, 
                                 xlab_in = labs_lst$age_title)+ 
  labs(title = paste0("b ", labs_lst$ci, ", ", labs_lst$sukup[val == 1, lab %>% firstlower()]))

fig_mr$c <- fig_mainresults_part(fig_dat_mr$ir[dg == "first" & sukup == 2], 
                                 ymax_in = 850, 
                                 ylab_in = labs_lst$ir_title, 
                                 xlab_in = labs_lst$age_title)+ 
  labs(title = paste0("c ", labs_lst$ir, ", ", labs_lst$sukup[val == 2, lab %>% firstlower()]))

fig_mr$d <- fig_mainresults_part(fig_dat_mr$ir[dg == "first" & sukup == 1], 
                                 ymax_in = 850, 
                                 ylab_in = labs_lst$ir_title, 
                                 xlab_in = labs_lst$age_title)+ 
  labs(title = paste0("d ", labs_lst$ir, ", ", labs_lst$sukup[val == 1, lab %>% firstlower()]))

fig_mr$e <- fig_mainresults_part(fig_dat_mr$pr[sukup == 2 & year == pr_year & dg == "first"], 
                                 ymax_in = 20, 
                                 ylab_in = labs_lst$pr_title, 
                                 xlab_in = labs_lst$age_title, 
                                 show_errorbar = F)+ 
  labs(title = paste0("e ", labs_lst$pr, ", ", labs_lst$sukup[val == 2, lab %>% firstlower()]))

fig_mr$f <- fig_mainresults_part(fig_dat_mr$pr[sukup == 1 & year == pr_year & dg == "first"], 
                                 ymax_in = 20, ylab_in = labs_lst$pr_title, 
                                 xlab_in = labs_lst$age_title, show_errorbar = F)+ 
  labs(title = paste0("f ", labs_lst$pr, ", ", labs_lst$sukup[val == 1, lab %>% firstlower()]))

## fig ----
# here just to take a look, will be put to gether in 1_mansucript

(fig_mr$a+fig_mr$b) /(fig_mr$c+fig_mr$d) / (fig_mr$e+fig_mr$f) +  plot_layout(guides = 'collect') & theme(legend.position = 'bottom')


# Fig dgs ----------------------------------------------------------------------

fig_dgs <- list()

## fun: draw dgs CI ------------------------------------------------------

# diagnosis wise

fig_dgs_part <- function(dat_in, dg_in, sukup_in, part_in, ymax_in = NA, show_errorbar = TRUE, 
                         ylab_in = labs_lst$ci_title, xlab_in = labs_lst$age_title){
  dat_in[dg == dg_in & sample == "all" ] %>% copy() %>%  
    .[,sukup := factor(sukup, levels = labs_lst$sukup$val, labels = labs_lst$sukup$lab)] %>% 
    .[, sample := factor(sample, levels = labs_lst$sample$val, labels = labs_lst$sample$lab)] %>% 
    ggplot(aes(y = est, x = x, color = factor(sukup)))+
    #geom_point()+
    #    {if(show_errorbar) geom_errorbar(aes(ymin =lower, ymax=upper), color = "gray", width = .5)}+
    {if(show_errorbar) geom_ribbon(aes(ymin =lower, ymax=upper, fill = sukup), alpha = .4, color = NA)}+
    labs(y = ylab_in, x = xlab_in, color = "", title = paste0(part_in), subtitle = dgs_tbl[dg == dg_in, labs_short])+
    geom_line()+
    ylim(c(0, ymax_in))+
    xlim(c(0, 100))+
    scale_color_nejm()+
    scale_fill_nejm(guide = "none")+
    theme(plot.subtitle = element_textbox_simple(halign = .05))
}

# see the occurence of different diagnoses for ordering (manually)
fig_dat_mr$ci[sample == "all" & time == 99][, max(est), dg][order(-V1)]

draw_fig_dgs <- function(dg1 = "f4" , dg2 = "f3" , dg3 = "f0", dg4 =  "f5", ymaxci = 80, ymaxir = 800, ymaxpr = 20){
  a_ci <- fig_dgs_part(fig_dat_mr$ci, dg_in = dg1, sukup_in = 2, part_in = paste0("a ", labs_lst$ci), ymax_in = ymaxci)
  a_ir <- fig_dgs_part(fig_dat_mr$ir, dg_in = dg1, sukup_in = 2, part_in = paste0("b ", labs_lst$ir), ymax_in = ymaxir, ylab_in = labs_lst$ir_title)
  a_pr <- fig_dgs_part(fig_dat_mr$pr[year == 2019 & x >0], dg_in = dg1, sukup_in = 2, part_in = paste0("c ", "Service utilization"), ymax_in = ymaxpr, 
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
                         labs(title = paste("a", labs_lst$sukup[val == 2, lab]))) + 
  (draw_compare_dgs_part(fig_dat_mr$ci, sukup_in = 1, ylab_in = labs_lst$ci_title, xlab_in = labs_lst$age_title) + 
     labs(title = paste("b", labs_lst$sukup[val == 1, lab]))) + plot_layout(guides = "collect")

fig_compare_dgs$ir <- (draw_compare_dgs_part(fig_dat_mr$ir, sukup_in = 2, ymax_in = 800, ylab_in = labs_lst$ir_title, xlab_in = labs_lst$age_title) + 
                         labs(title = paste("a", labs_lst$sukup[val == 2, lab]))) + 
  (draw_compare_dgs_part(fig_dat_mr$ir, sukup_in = 1, ymax_in = 800, ylab_in = labs_lst$ir_title, xlab_in = labs_lst$age_title) + 
     labs(title = paste("b", labs_lst$sukup[val == 1, lab])))


# SAVE figures -----------------------------------------------------------------

# ((fig_mr$a+fig_mr$b) /(fig_mr$c+fig_mr$d) / (fig_mr$e+fig_mr$f) +  plot_layout(guides = 'collect') & theme(legend.position = 'bottom')) %>% 
#   ggsave(here("submissions", "fig1.pdf"), plot = ., device = "pdf", dpi = 600, width = 9.1, height = 8)
# 
# fig_dgs$part1 %>% ggsave(here("submissions", "fig2.pdf"), plot = ., device = "pdf", dpi = 600, width = 9.1, height = 8)
# fig_dgs$part2 %>% ggsave(here("submissions", "fig3.pdf"), plot = ., device = "pdf", dpi = 600, width = 9.1, height = 8)
# fig_dgs$part3 %>% ggsave(here("submissions", "fig4.pdf"), plot = ., device = "pdf", dpi = 600, width = 9.1, height = 8)
