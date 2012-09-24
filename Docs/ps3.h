rdesc = {
 0x05, 0x01,          //  GUsagePage Generic Desktop
 0x09, 0x04,          //  LUsage 0x04 [Joystick]
 0xA1, 0x01,          //  MCollection Application (mouse, keyboard)
  0x09, 0x04,         //  LUsage 0x04 [Joystick]
  0xA1, 0x02,         //  MCollection Logical (interrelated data)
   0x85, 0x01,        //  GReportID 0x01 [1]
   0x75, 0x08,        //  GReportSize 0x08 [8]
   0x95, 0x01,        //  GReportCount 0x01 [1]
   0x80,              //  MInput 
                      //  

   0x05, 0x09,        //  GUsagePage Button
   0x75, 0x01,        //  GReportSize 0x01 [1]
   0x95, 0x04,        //  GReportCount 0x04 [4]
   0x14,              //  GLogicalMinimum  [0]
   0x25, 0x01,        //  GLogicalMaximum 0x01 [1]
   0x09, 0x0C,        //  LUsage 0x0C [Button 0C]
   0x09, 0x0A,        //  LUsage 0x0A [Button 0A]
   0x09, 0x09,        //  LUsage 0x09 [Button 09]
   0x09, 0x0B,        //  LUsage 0x0B [Button 0B]
   0x81, 0x02,        //  MInput 0x02
                      //  Data[0] Var[1] Abs[2] 

   0x05, 0x01,        //  GUsagePage Generic Desktop
   0x09, 0x01,        //  LUsage 0x01 [Pointer]
   0xA1, 0x02,        //  MCollection Logical (interrelated data)
    0x75, 0x01,       //  GReportSize 0x01 [1]
    0x14,             //  GLogicalMinimum  [0]
    0x25, 0x01,       //  GLogicalMaximum 0x01 [1]
    0x95, 0x04,       //  GReportCount 0x04 [4]
    0x09, 0x90,       //  LUsage 0x90 [D-pad Up]
    0x09, 0x92,       //  LUsage 0x92 [D-pad Right]
    0x09, 0x91,       //  LUsage 0x91 [D-pad Down]
    0x09, 0x93,       //  LUsage 0x93 [D-pad Left]
    0x81, 0x02,       //  MInput 0x02
                      //  Data[0] Var[1] Abs[2] 

    0xC0,             //  MEndCollection  [Pointer]

   0x05, 0x09,        //  GUsagePage Button
   0x95, 0x09,        //  GReportCount 0x09 [9]
   0x09, 0x08,        //  LUsage 0x08 [Button 08]
   0x09, 0x07,        //  LUsage 0x07 [Button 07]
   0x09, 0x06,        //  LUsage 0x06 [Button 06]
   0x09, 0x05,        //  LUsage 0x05 [Button 05]
   0x09, 0x04,        //  LUsage 0x04 [Button 04]
   0x09, 0x02,        //  LUsage 0x02 [Button 2 (secondary)]
   0x09, 0x01,        //  LUsage 0x01 [Button 1 (primary/trigger)]
   0x09, 0x03,        //  LUsage 0x03 [Button 3 (tertiary)]
   0x09, 0x0D,        //  LUsage 0x0D [Button 0D]
   0x81, 0x02,        //  MInput 0x02
                      //  Data[0] Var[1] Abs[2] 

   0x75, 0x01,        //  GReportSize 0x01 [1]
   0x95, 0x0F,        //  GReportCount 0x0F [15]
   0x80,              //  MInput 
                      //  

   0x14,              //  GLogicalMinimum  [0]
   0x26, 0xFF, 0x00,  //  GLogicalMaximum 0x00FF [255]
   0x35, 0x80,        //  GPhysicalMinimum 0x80 [-128]
   0x45, 0x7F,        //  GPhysicalMaximum 0x7F [127]
   0x05, 0x01,        //  GUsagePage Generic Desktop
   0x09, 0x01,        //  LUsage 0x01 [Pointer]
   0x75, 0x08,        //  GReportSize 0x08 [8]
   0x95, 0x02,        //  GReportCount 0x02 [2]
   0xA0,              //  MCollection Physical (group of axes)
    0x09, 0x30,       //  LUsage 0x30 [X]
    0x09, 0x31,       //  LUsage 0x31 [Y]
    0x81, 0x02,       //  MInput 0x02
                      //  Data[0] Var[1] Abs[2] 

    0xC0,             //  MEndCollection  [Pointer]

   0x09, 0x05,        //  LUsage 0x05 [Game Pad]
   0xA0,              //  MCollection Physical (group of axes)
    0x09, 0x32,       //  LUsage 0x32 [Z]
    0x09, 0x33,       //  LUsage 0x33 [Rx]
    0x81, 0x02,       //  MInput 0x02
                      //  Data[0] Var[1] Abs[2] 

    0xC0,             //  MEndCollection  [Game Pad]

   0x75, 0x08,        //  GReportSize 0x08 [8]
   0x95, 0x04,        //  GReportCount 0x04 [4]
   0x80,              //  MInput 
                      //  

   0x75, 0x08,        //  GReportSize 0x08 [8]
   0x95, 0x0C,        //  GReportCount 0x0C [12]
   0x09, 0x46,        //  LUsage 0x46 [Vno]
   0x34,              //  GPhysicalMinimum  [0]
   0x44,              //  GPhysicalMaximum  [0]
   0x81, 0x02,        //  MInput 0x02
                      //  Data[0] Var[1] Abs[2] 

   0x75, 0x08,        //  GReportSize 0x08 [8]
   0x95, 0x0F,        //  GReportCount 0x0F [15]
   0x80,              //  MInput 
                      //  

   0x75, 0x10,        //  GReportSize 0x10 [16]
   0x95, 0x01,        //  GReportCount 0x01 [1]
   0x16, 0x80, 0x01,  //  GLogicalMinimum 0x0180 [384]
   0x26, 0x7F, 0x02,  //  GLogicalMaximum 0x027F [639]
   0x45, 0x80,        //  GPhysicalMaximum 0x80 [-128]
   0x35, 0x7F,        //  GPhysicalMinimum 0x7F [127]
   0x09, 0x33,        //  LUsage 0x33 [Rx]
   0x81, 0x02,        //  MInput 0x02
                      //  Data[0] Var[1] Abs[2] 

   0x35, 0x80,        //  GPhysicalMinimum 0x80 [-128]
   0x45, 0x7F,        //  GPhysicalMaximum 0x7F [127]
   0x09, 0x34,        //  LUsage 0x34 [Ry]
   0x81, 0x02,        //  MInput 0x02
                      //  Data[0] Var[1] Abs[2] 

   0x95, 0x02,        //  GReportCount 0x02 [2]
   0x14,              //  GLogicalMinimum  [0]
   0x26, 0x00, 0x04,  //  GLogicalMaximum 0x0400 [1024]
   0x36, 0x01, 0xFE,  //  GPhysicalMinimum 0xFE01 [-511]
   0x46, 0x00, 0x02,  //  GPhysicalMaximum 0x0200 [512]
   0x09, 0x35,        //  LUsage 0x35 [Rz]
   0x09, 0x36,        //  LUsage 0x36 [Slider]
   0x81, 0x02,        //  MInput 0x02
                      //  Data[0] Var[1] Abs[2] 

   0x14,              //  GLogicalMinimum  [0]
   0x26, 0xFF, 0x00,  //  GLogicalMaximum 0x00FF [255]
   0x34,              //  GPhysicalMinimum  [0]
   0x46, 0xFF, 0x00,  //  GPhysicalMaximum 0x00FF [255]
   0x75, 0x08,        //  GReportSize 0x08 [8]
   0x95, 0x30,        //  GReportCount 0x30 [48]
   0x91, 0x02,        //  MOutput 0x02
                      //  Data[0] Var[1] Abs[2] 

   0x75, 0x08,        //  GReportSize 0x08 [8]
   0x95, 0x30,        //  GReportCount 0x30 [48]
   0xB1, 0x02,        //  MFeature 0x02
                      //  Data[0] Var[1] Abs[2] 

   0xC0,              //  MEndCollection  [Joystick]

  0xA1, 0x02,         //  MCollection Logical (interrelated data)
   0x85, 0x02,        //  GReportID 0x02 [2]
   0x75, 0x08,        //  GReportSize 0x08 [8]
   0x95, 0x30,        //  GReportCount 0x30 [48]
   0xB1, 0x02,        //  MFeature 0x02
                      //  Data[0] Var[1] Abs[2] 

   0xC0,              //  MEndCollection  [Joystick]

  0xA1, 0x02,         //  MCollection Logical (interrelated data)
   0x85, 0xEE,        //  GReportID 0xEE [238]
   0x75, 0x08,        //  GReportSize 0x08 [8]
   0x95, 0x30,        //  GReportCount 0x30 [48]
   0xB1, 0x02,        //  MFeature 0x02
                      //  Data[0] Var[1] Abs[2] 

   0xC0,              //  MEndCollection  [Joystick]

  0xA1, 0x02,         //  MCollection Logical (interrelated data)
   0x85, 0xEF,        //  GReportID 0xEF [239]
   0x75, 0x08,        //  GReportSize 0x08 [8]
   0x95, 0x30,        //  GReportCount 0x30 [48]
   0xB1, 0x02,        //  MFeature 0x02
                      //  Data[0] Var[1] Abs[2] 

   0xC0,              //  MEndCollection  [Joystick]

  0xC0                //  MEndCollection  [Joystick]

}
