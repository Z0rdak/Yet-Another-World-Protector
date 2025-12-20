package de.z0rdak.yawp.config.server;

import net.minecraftforge.common.ForgeConfigSpec;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import static de.z0rdak.yawp.constants.Constants.MOD_ID;

public class FeatureConfig {

    public static final ForgeConfigSpec CONFIG_SPEC;
    public static final String CONFIG_NAME = MOD_ID + "-features.toml";
    public static final Logger FEATURE_CONFIG_LOGGER = LogManager.getLogger(MOD_ID.toUpperCase() + "-Feature-Config");
    public static final ForgeConfigSpec.ConfigValue<Boolean> PLAYER_TRACKER;
    public static final ForgeConfigSpec.ConfigValue<Boolean> AUTO_CREATE_NEW_LEVEL_DATA;

    static {
        final ForgeConfigSpec.Builder BUILDER = new ForgeConfigSpec.Builder();

        BUILDER.push("YAWP feature config").build();

        PLAYER_TRACKER = BUILDER.comment("Enable player position tracker")
                .define("player_tracker", false);
        BUILDER.pop();

        AUTO_CREATE_NEW_LEVEL_DATA = BUILDER.comment("Enables automatic creation of Dimensional Regions for new levels")
                .define("auto_create_new_level_data", true);
        BUILDER.pop();
        CONFIG_SPEC = BUILDER.build();
    }

    public static boolean enablePlayerTracker() {
        return PLAYER_TRACKER.get();
    }

    public static boolean shouldCreateNewLevelData() {
        return AUTO_CREATE_NEW_LEVEL_DATA.get();
    }
}