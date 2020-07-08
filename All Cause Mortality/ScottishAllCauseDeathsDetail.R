tiff("Outputs/NRSWeeklyDeathsxAge.tiff", units="in", width=12, height=8, res=300)
ggplot(data.age)+
  geom_ribbon(aes(x=week, ymin=min, ymax=max), fill="Skyblue2")+
  geom_ribbon(aes(x=week, ymin=mean, ymax=deaths), fill="Red", alpha=0.2)+
  geom_line(aes(x=week, y=mean), colour="Grey50", linetype=2)+
  geom_line(aes(x=week, y=deaths), colour="Red")+
  theme_classic()+
  facet_wrap(~age)+
  scale_x_continuous(name="Week number", breaks=c(0,10,20,30,40,50))+
  scale_y_continuous(name="Deaths registered")+
  expand_limits(y=0)+
  labs(title="Deaths from all causes are looking 'normal' at all ages",
       subtitle=paste0("Weekly deaths in <span style='color:red;'>2020</span> compared to <span style='color:Skyblue4;'>the range in 2010-19</span>. Data up to ", ScotDate, "."),
       caption="Data from NRS | Plot by @VictimOfMaths")+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        plot.subtitle =element_markdown())+
  geom_text(data=ann_text2, aes(x=week, y=pos), label=c(paste0(round(excess.age[1,2],0)," excess deaths in 2020\nvs. 2010-19 average (",
                                                               round(excess.age[1,4]*100, 0),"%)"), 
                                                        paste0("+",round(excess.age[2,2],0)," deaths (+",
                                                               round(excess.age[2,4]*100, 0),"%)"),
                                                        paste0("+",round(excess.age[3,2],0)," deaths (+",
                                                               round(excess.age[3,4]*100, 0),"%)"),
                                                        paste0("+",round(excess.age[4,2],0)," deaths (+",
                                                               round(excess.age[4,4]*100, 0),"%)"),
                                                        paste0("+",round(excess.age[5,2],0)," deaths (+",
                                                               round(excess.age[5,4]*100, 0),"%)"),
                                                        paste0("+",round(excess.age[6,2],0)," deaths (+",
                                                               round(excess.age[6,4]*100, 0),"%)")), 
            size=3, colour=rep("red", times=6), hjust=0)

dev.off()
