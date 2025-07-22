package de.z0rdak.yawp;

public interface YAWPModInitializer {

    void registerCommands();
    void setupRegionDataLifecycleHooks();
    void registerConfig();
}
