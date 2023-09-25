package de.z0rdak.yawp.config.server;

import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.core.flag.FlagCategory;
import de.z0rdak.yawp.core.flag.RegionFlag;
import net.minecraftforge.common.ForgeConfigSpec;

import java.util.*;
import java.util.stream.Collectors;

public class FlagConfig {

    public static final ForgeConfigSpec CONFIG_SPEC;
    public static final String CONFIG_NAME = YetAnotherWorldProtector.MODID + "-flags.toml";

    private static final ForgeConfigSpec.ConfigValue<Boolean> REMOVE_ENTITIES_FOR_SPAWNING_FLAGS;
    private static final ForgeConfigSpec.ConfigValue<List<? extends String>> BREAK_FLAG_ENTITIES;
    private static final ForgeConfigSpec.ConfigValue<List<? extends String>> BREAK_FLAG_ENTITY_TAGS;
    private static final ForgeConfigSpec.ConfigValue<String> LOCAL_DEFAULT_FLAG_MSG;
    private static final ForgeConfigSpec.ConfigValue<String> DIM_DEFAULT_FLAG_MSG;
    private static final ForgeConfigSpec.ConfigValue<String> GLOBAL_DEFAULT_FLAG_MSG;
    private static final ForgeConfigSpec.ConfigValue<Boolean> ENABLE_FLAG_INHERITANCE;
    private static final SortedMap<RegionFlag, ForgeConfigSpec.ConfigValue<String>> DEFAULT_FLAG_MESSAGES = new TreeMap<>();

    private static final String DEFAULT_FLAG_MSG_PATH = "YAWP-default-flag-message-configuration";

    private static final String DEFAULT_PLAYER_SPECIFIC_MSG = "The '{flag}' flag denies this action here!";

    static {
        final ForgeConfigSpec.Builder BUILDER = new ForgeConfigSpec.Builder();

        BUILDER.push("YetAnotherWorldProtector mod flag configuration").build();

        BREAK_FLAG_ENTITIES = BUILDER
                .comment("Entities included/protected by the break block flag. Includes entities like armor stands and pictures by default")
                .defineListAllowEmpty(Collections.singletonList("break_flag_entities"), FlagConfig::defaultEntityBreakFlagEntries, FlagConfig::isValidEntityEntry);

        BREAK_FLAG_ENTITY_TAGS = BUILDER
                .comment("Entity tags included/protected by the break block flag.")
                .defineListAllowEmpty(Collections.singletonList("break_flag_entity_tags"), ArrayList::new, FlagConfig::isValidTagEntry);

        REMOVE_ENTITIES_FOR_SPAWNING_FLAGS = BUILDER
                .comment("Toggle to remove entities when adding spawning-* flags.\n true -> remove entities related to this flag\n false -> don't remove entities")
                .define(Collections.singletonList("remove_entities_for_spawning_flags"), true);

        LOCAL_DEFAULT_FLAG_MSG = BUILDER
                .comment("Default flag message for Local Regions. Displayed when a flag action is denied.")
                .define(Collections.singletonList("local_flag_msg"), "[{region}]: The '{flag}' flag denies this action here!");

        DIM_DEFAULT_FLAG_MSG = BUILDER
                .comment("Default flag message for Dimensional Regions. Displayed when a flag action is denied.")
                .define(Collections.singletonList("dim_flag_msg"), "[{region}]: The '{flag}' flag denies this action in this dimension!");

        GLOBAL_DEFAULT_FLAG_MSG = BUILDER
                .comment("Default flag message for the Global Region. Displayed when a flag action is denied.")
                .define(Collections.singletonList("global_flag_msg"), "[This action is globally denied because of the '{flag}' flag!");

        ENABLE_FLAG_INHERITANCE = BUILDER.comment("Enable flag inheritance.")
                .define("enable_flag_inheritance", false);

        BUILDER.pop();

        BUILDER.push(DEFAULT_FLAG_MSG_PATH).build();

        RegionFlag.getFlagsMatchingCategory(FlagCategory.PLAYER)
                .stream()
                .sorted(Comparator.comparing(f -> f.name))
                .forEach(flag -> {
                    ForgeConfigSpec.ConfigValue<String> flagMsgConfig = BUILDER
                            .comment("Default flag message for flag '" + flag.name + "'.")
                            .define(Collections.singletonList(flag.name), DEFAULT_PLAYER_SPECIFIC_MSG);
                            DEFAULT_FLAG_MESSAGES.put(flag, flagMsgConfig);
                        }
                );
        BUILDER.pop();

        CONFIG_SPEC = BUILDER.build();
    }

    public static boolean isFlagInheritanceEnabled() {
        return ENABLE_FLAG_INHERITANCE.get();
    }

    public static String getDefaultFlagMessage(RegionFlag flag) {
        return DEFAULT_FLAG_MESSAGES.get(flag).get();
    }

    public static boolean hasDefaultMsgConfig(RegionFlag flag) {
        return DEFAULT_FLAG_MESSAGES.containsKey(flag);
    }

    private static List<String> defaultEntityBreakFlagEntries() {
        return Arrays.asList("minecraft:armor_stand", "minecraft:painting", "minecraft:item_frame", "minecraft:glow_item_frame", "minecraft:leash_knot");
    }

    public static Set<String> getBreakFlagEntities() {
        return BREAK_FLAG_ENTITIES.get().stream()
                .filter(Objects::nonNull)
                .map(String::toString).collect(Collectors.toSet());
    }

    public static Set<String> getBreakFlagEntityTags() {
        return BREAK_FLAG_ENTITY_TAGS.get().stream()
                .filter(Objects::nonNull)
                .map(String::toString).collect(Collectors.toSet());
    }

    public static boolean removeEntitiesEnabled() {
        return REMOVE_ENTITIES_FOR_SPAWNING_FLAGS.get();
    }

    public static String getRawDimFlagMsg() {
        return DIM_DEFAULT_FLAG_MSG.get();
    }

    public static String getRawGlobalFlagMsg() {
        return GLOBAL_DEFAULT_FLAG_MSG.get();
    }

    public static String getRawLocalFlagMsg() {
        return LOCAL_DEFAULT_FLAG_MSG.get();
    }

    public static boolean isDefinedInConfig(RegionFlag regionFlag) {
        return DEFAULT_FLAG_MESSAGES.containsKey(regionFlag);
    }

    public static boolean hasDefaultFlagMsg(RegionFlag regionFlag) {
        return isDefinedInConfig(regionFlag) && DEFAULT_FLAG_MESSAGES.get(regionFlag).get().equals(DEFAULT_PLAYER_SPECIFIC_MSG);
    }

    public static boolean hasCustomFlagMsg(RegionFlag regionFlag) {
        return isDefinedInConfig(regionFlag) && !DEFAULT_FLAG_MESSAGES.get(regionFlag).get().equals(DEFAULT_PLAYER_SPECIFIC_MSG);
    }

    private static boolean isValidEntityEntry(Object entity) {
        return true;
    }

    /**
     * TODO: Implementation for validation of tags
     *
     * @param tags
     * @return
     */
    private static boolean isValidTagEntry(Object tags) {
        return true;
    }

    /**
     * TODO: Implementation for validation flag msg
     *
     * @param flagMsg
     * @return
     */
    private static boolean isValidFlagMsg(Object flagMsg) {
        return true;
    }
}
