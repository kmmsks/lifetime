library(tibble)

draw_washout <- function(study_start = tms$start, study_end = tms$end, sensitivity = F, sensitivity_primarycare_washout = 1,
                         inpat_start = tms$inpat, outpat_start = tms$outpat, primarycare_start = tms$prim,
                         birthy = 1960, dataline_col = "gray", annotation_pos1 = 12.1, annotation_pos_upper_row = 12.7,
                         period_colors = c("gray90", "lightblue", "deepskyblue", "black", "pink"), override_period_color5 = "pink",
                         period_alphas = c(1, .6, .6,1, .9), text_size = 8, title_in = "add title"){
  #end point sets the time that censurations are in relation to
  end_point <- (study_end - primarycare_start)/2 + primarycare_start +2
  end_x <- c(end_point -4, end_point -2, end_point)
  # censuration types
  end_contact <- c("inpat", "outpat", "prim")
  
  # lost from follow up is set to this time:
  cens_lost <- end_point
  
  #washout:  time to demonstrate normal washout
  #main analysis
  cens_inpat_wo  <- (study_start - inpat_start)/2 + inpat_start
  cens_outpat_wo <- (study_start - outpat_start)/2 + outpat_start
  
  #sensitivity analyses:
  period_colors[5] <- override_period_color5
  
  # locations ot contatcs that are not caputered by washouts
  not_correct_contacts <- 
    tibble::tribble(
      ~id, ~inpat, ~outpat, ~prim,
      6,   inpat_start-9, NA, NA, 
      7,   inpat_start-7, outpat_start-7, NA,
      8,   inpat_start-5, outpat_start-5, primarycare_start -5,
    ) %>% setDT() %>% melt(measure.vars=c("inpat", "outpat", "prim"), variable.name = "contact", value.name = "time") %>% 
    .[, id:= id %>% as.factor()]
  
  list(
    correct = data.table(end = c(end_x, cens_lost, study_end), contact = c(end_contact, "lost", "end")),
    not_correct = data.table(end = end_x, contact = end_contact),
    wo = data.table(end = c(cens_inpat_wo, cens_outpat_wo), contact = end_contact[1:2]),
    sens = data.table(end =c(primarycare_start + .5), contact = end_contact[3])
    ) %>% 
    rbindlist(idcol = "part") %>% 
    .[, `:=`(id = seq(.N) %>% factor(), start = birthy)] %>% 
    melt(measure.vars=c("start", "end"), variable.name = "type", value.name = "time") %>% 
    ggplot(aes(x = id , y = time, 
               shape = factor(contact, 
                              levels = c("inpat", "outpat", "prim", "lost", "end"),
                              labels = c("Inpatient", "Outpatient", "Primary care", "Death, emigration", "End of  study"))))+
    geom_line(data = data.table(x = c(-Inf, 11.5), y = c(tms$prim, primarycare_start)), aes(x = x , y = y), linetype = "dashed", color = dataline_col, inherit.aes = F) +
    annotate("rect", ymin = birthy, ymax = inpat_start, xmin = 0, xmax = 11.5, alpha = period_alphas[1], fill = period_colors[1])+
    annotate("rect", ymin = inpat_start, ymax = study_start, xmin = 0, xmax = 11.5, alpha = period_alphas[2], fill = period_colors[2])+
    annotate("rect", ymin = outpat_start, ymax = study_start, xmin = 0, xmax = 11.5, alpha = period_alphas[3], fill = period_colors[3])+
    {if(sensitivity)annotate("rect", 
                             ymin = primarycare_start, ymax = primarycare_start + sensitivity_primarycare_washout, 
                             xmin = 0, xmax = 11.5,  alpha = period_alphas[5], fill = period_colors[5], color = period_colors[5])}+
    annotate("rect", ymin = study_start, ymax = study_end, xmin = 0, xmax = 11.5, alpha = 0, color= period_colors[4])+
    annotate("label", x = annotation_pos1, y= (inpat_start- birthy)/2 + birthy, size = text_size/.pt, 
             label= "Period not covered", alpha = period_alphas[1], fill = period_colors[1])+
    annotate("label", x= annotation_pos_upper_row, y= (outpat_start- inpat_start)/2 + inpat_start, size = text_size/.pt, 
             label= "Inpatient \nwashout",  alpha = period_alphas[2], fill = period_colors[2])+
    annotate("label", x=  annotation_pos_upper_row + .4, y= (study_start- outpat_start)/2 + outpat_start, size = text_size/.pt, 
             label= "Inpatient and\n outpatient \nwashout",  alpha = period_alphas[3], fill = period_colors[3])+
    annotate("text", x= annotation_pos1-.05, y= (study_end- study_start)/2 + study_start, size = text_size /.pt, 
             label= "Study period")+
    geom_point(data = function(x) x[type == "end"], size = 2)+
    geom_line(color = "black", linetype = "dashed")+
    geom_point(data = not_correct_contacts, size = 2)+
    geom_line(data = function(x) x[part == "correct"][type == "start", time := study_start], color = "black")+
    geom_line(data = function(x) x[part == "not_correct"][type == "start", time := study_start], color = "red")+
    scale_y_continuous(limits =  c(birthy-0, NA), 
                       breaks = c(birthy, inpat_start, outpat_start, primarycare_start, study_start, study_end),
                       labels = c("date of \nbirth", inpat_start, outpat_start, primarycare_start, paste0("\n", study_start), paste0("\n", study_end)))+
    scale_x_discrete(limits = rev, expand=c(0,0,.36,0))+
    coord_flip()+
    theme_classic()+
    labs(x = "Person ID", y = "Year", shape = "", title = title_in)
}

# Put together:
fig_washout <- function(){
  a <- draw_washout(title_in = "a Main Analysis") +geom_line(data = function(x) x[part == "sens"][type == "start", time := tms$start], color = "black")
  
  b <- draw_washout(study_start = tms$sens1_start, sensitivity = T, title_in = "b Sensitivity Analysis with a Three-Year Extra Washout and \nRetrospective Washout in Primary Care") + 
    annotate("label", x=  13.5 , y= 2012, size = 8/.pt, 
             label= "Primary care \nwashout",  alpha = .9, fill = "pink")
  
  c <- draw_washout(study_start = tms$sens2_start, sensitivity = T, override_period_color5 = "orange", title_in = "c Sensitivity Analysis with Restricted Follow-up Time") + 
    annotate("label", x=  13.5 , y= 2016, size = 8/.pt, 
             label= "Complete \nwashout",  alpha = .9, fill = "orange")
  
  a/ b / c
}


