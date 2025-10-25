package de.z0rdak.yawp.mixin.flag;

public record BlockBreakResult(boolean canceled, int xp) {
    public static final BlockBreakResult PASS = new BlockBreakResult(false, 0);
}