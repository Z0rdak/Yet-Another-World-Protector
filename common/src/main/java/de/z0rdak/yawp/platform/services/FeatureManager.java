package de.z0rdak.yawp.platform.services;

public interface FeatureManager {

    /**
     * Set up event handlers to track player position on player tick and
     * to clear player position cache on disconnect to clean up memory
     */
    void enablePlayerTracker();
    boolean shouldCreateNewLevelData();
}
