package com.us.dsb.explore.algs.coloredlines.manual.ui.tapapi

/** Interpreted meaning/command of a (virtual) tap on a board cell. */
private[ui] sealed trait TapAction

private[ui] object TapAction {

  /** Tap selects cell that has ball. */
  private[ui] case object SelectBallTap  extends TapAction

  /** Tap selects cell that is empty. */
  private[ui] case object SelectEmptyTap extends TapAction

  /** Tap cancels selection. */
  private[ui] case object DeselectTap    extends TapAction

  /** Tap makes game move to move previously selected ball (if open path) */
  private[ui] case object TryMoveBallTap extends TapAction

  /** Tap makes game move to pass (move nothing and get more balls placed). */
  private[ui] case object PassTap        extends TapAction
}
