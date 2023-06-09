package com.us.dsb.explore.algs.coloredlines.manual.ui.tapapi

/** Interpreted meaning/command of a (virtual) tap on a board cell. */
private[ui] sealed trait TapCase

private[ui] object TapCase {

  /** Tap selects cell that has ball. */
  private[ui] case object SelectBallTap  extends TapCase

  /** Tap selects cell that is empty. */
  private[ui] case object SelectEmptyTap extends TapCase

  /** Tap cancels selection. */
  private[ui] case object DeselectTap    extends TapCase

  /** Tap makes game move to move previously selected ball (if open path) */
  private[ui] case object TryMoveBallTap extends TapCase

  /** Tap makes game move to pass (move nothing and get more balls placed). */
  private[ui] case object PassTap        extends TapCase
}
