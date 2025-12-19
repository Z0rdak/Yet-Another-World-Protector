package de.z0rdak.yawp.api.events;

public interface Cancelable {
    boolean isCanceled();
    void setCanceled(boolean canceled);

    public static final boolean CANCEL = false;
    public static final boolean CONTINUE = true;
}