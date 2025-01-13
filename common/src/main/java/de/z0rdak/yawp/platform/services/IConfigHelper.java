package de.z0rdak.yawp.platform.services;

import net.neoforged.neoforge.common.ModConfigSpec;

public interface IConfigHelper {
    void registerServerConfig(ModConfigSpec spec, String configName);
    void registerClientConfig(ModConfigSpec spec, String configName);
    void registerCommonConfig(ModConfigSpec spec, String configName);
    void register();
}
