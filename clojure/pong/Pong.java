//
// Copyright (c) 2009, Justin Grant <justin at imagine27 dot com>
// All rights reserved.

// Redistribution and use in source and binary forms, with or without modification,
// are permitted provided that the following conditions are met:

// Redistributions of source code must retain the above copyright notice, this list
// of conditions and the following disclaimer.
// Redistributions in binary form must reproduce the above copyright notice, this
// list of conditions and the following disclaimer in the documentation and/or
// other materials provided with the distribution.
// Neither the name of the <ORGANIZATION> nor the names of its contributors may be
// used to endorse or promote products derived from this software without specific
// prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
// ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
// ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
// (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
// LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
// NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
// EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//


/**
 * Pong Game
 */


import java.awt.*;
import java.awt.image.*;
import java.awt.event.*;
import javax.swing.*;

public class Pong extends JFrame implements MouseMotionListener, KeyListener {

  int my,bx,by,px,py,compx,compy,width,height,speedx,speedy,bwidth,bheight,pwidth,pheight,score;
  boolean started;
  private Timer timer1;

  public static void main(String args[])
  {
    Pong game = new Pong();
    game.init();
  }

  public void init() {
    Toolkit tk = Toolkit.getDefaultToolkit();
    setBounds(0, 0,
              tk.getScreenSize().width,
              tk.getScreenSize().height);
    width = getSize().width;
    height = getSize().height;
    setExtendedState(Frame.MAXIMIZED_BOTH);
    setUndecorated(true);
    setVisible(true);
    createBufferStrategy(2);
    setBackground(Color.BLACK);
    getContentPane().setBackground(Color.BLACK);
    pheight = 160;
    pwidth = 30;
    bheight = 30;
    bwidth = 30;
    addKeyListener(this);
    addMouseMotionListener(this);
    px = 35;
    compx = width - 35 - pwidth;
    newgame();

    timer1 = new Timer(10,
                       new ActionListener() {
                         public void actionPerformed(ActionEvent e) {
                           height = getSize().height;
                           width = getSize().width;
                           bx += speedx;
                           by += speedy;
                           if (by <= 0 || by + bheight >= height) {
                             speedy = -speedy;
                           }
                           if (bx <= px + pwidth &&
                               by + bheight >= py &&
                               by <= py + pheight && bx > px) {
                             speedx = -speedx;
                             ++score;
                             // Toolkit.getDefaultToolkit().beep();
                             // System.out.println("\007");
                           }
                           if (bx + bwidth >= compx && by + bheight >= compy &&
                               by <= compy + pheight && bx < compx + pwidth) {
                             speedx = -speedx;
                           }
                           if (speedx < 0) {
                             if (compy + pheight / 2 != height / 2) {
                               if (compy + pheight / 2 > height / 2) {
                                 compy -= -speedx;
                               }
                               else {
                                 compy += -speedx;
                               }
                             }
                           }
                           else {
                             if (by + bheight / 2 <= compy + pheight / 2) {
                               compy -= speedx;
                             }
                             else {
                               compy += speedx;
                             }
                           }
                           if (compy < 0) {
                             compy = 0;
                           }
                           if (compy + pheight > height) {
                             compy = height - pheight;
                           }
                           if (bx + bwidth < 0) {
                             py = height / 2 - pheight / 2;
                             timer1.stop();
                             started = false;
                           }

                           repaint();

                         }
                       });
  }

  public void mouseMoved(MouseEvent e) {
    if (started) {
      my = e.getY();
      if (my + pheight / 2 > height) {
        my = height - pheight / 2;
      }
      if (my < pheight / 2) {
        my = pheight / 2;
      }
      py = my - pheight / 2;
      repaint();
    }
  }

  public void mouseDragged(MouseEvent e) { }

  public void paint(Graphics g) {
    super.paint(g);
    Font font1 = new Font("Courier New", Font.BOLD, 24);
    setBackground(Color.BLACK);
    getContentPane().setBackground(Color.BLACK);
    g.setColor(Color.WHITE);
    //g.drawRect(0, 0, width - 1, height - 1);
    g.fillRect(px, py, pwidth, pheight);
    g.fillRect(compx, compy, pwidth, pheight);
    g.setFont(font1);
    g.drawString("SCORE " + score, 20, 20);
    if (started) {
      g.fillRect(bx, by, bwidth, bheight);
    }
    else {
      Font font2 = new Font("Courier New", Font.BOLD, 40);
      g.setFont(font2);
      getContentPane().setBackground(Color.BLACK);
      g.setColor(Color.WHITE);
      g.drawString("PONG", width / 2 - 46, height / 2 - 16);
      g.setFont(font1);
      g.drawString("PRESS 'S' TO START", width / 2 - 100, height / 2 + 30);
    }
  }

  public void newgame() {
    py = height / 2 - pheight / 2;
    compy = py;
    bx = width / 2 - bwidth / 2;
    by = height / 2 - bheight / 2;
    speedx = 10;
    speedy = 10;
    score = 0;
  }

  public void keyPressed(KeyEvent e) {
    if (e.getKeyChar() == 's') {
      started = true;
      newgame();
      timer1.start();
    }
  }

  public void keyTyped(KeyEvent e) { }

  public void keyReleased(KeyEvent e) { }

}

