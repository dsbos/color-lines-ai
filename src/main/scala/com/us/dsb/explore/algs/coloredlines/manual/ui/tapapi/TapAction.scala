package com.us.dsb.explore.algs.coloredlines.manual.ui.tapapi

/** Interpreted meaning/command of a (virtual) tap on a board cell. */
private[ui] sealed trait TapAction

private[ui] object TapAction {

  /** Select tapped-on cell that has ball. */
  private[ui] case object SelectBall  extends TapAction

  /** Select tapped-on cell that is empty. */
  private[ui] case object SelectEmpty extends TapAction

  /** Cancel selection. */
  private[ui] case object Deselect    extends TapAction

  /** (Try to) move ball from selected cell to tapped-on empty cell (if open path). */
  private[ui] case object TryMoveBall extends TapAction  //???? should this capture, carry coords?

  /** Pass (move nothing, get more balls placed). */
  private[ui] case object Pass        extends TapAction
}
