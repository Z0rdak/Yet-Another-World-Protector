package de.z0rdak.yawp.config.server;

import de.z0rdak.yawp.api.FlagTagRegister;
import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.api.events.region.FlagCheckResult;
import de.z0rdak.yawp.api.events.region.ForgeFlagCheckEvent;
import de.z0rdak.yawp.api.events.region.ForgeFlagCheckResult;
import de.z0rdak.yawp.constants.Constants;
import de.z0rdak.yawp.core.flag.*;
import de.z0rdak.yawp.core.region.RegionType;
import de.z0rdak.yawp.util.AreaUtil;
import net.minecraft.resources.ResourceLocation;
import net.minecraftforge.common.ForgeConfigSpec;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.*;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.constants.Constants.MOD_ID;

public class LoggingConfig {

    public static final ForgeConfigSpec CONFIG_SPEC;
    public static final String CONFIG_NAME = Constants.MOD_ID + "-logging.toml";
    public static final Logger LOGGING_CONFIG_LOGGER = LogManager.getLogger(MOD_ID.toUpperCase() + "-Logging-Config");

    private static final ForgeConfigSpec.ConfigValue<Boolean> FLAG_CHECK_LOG;
    private static final ForgeConfigSpec.ConfigValue<Boolean> FLAG_RESULT_LOG;
    private static final ForgeConfigSpec.ConfigValue<Boolean> LOG_EMPTY_RESULTS;
    // private static final ForgeConfigSpec.ConfigValue<Boolean> DETAILED_PLAYER_FLAG_LOG;
    private static final ForgeConfigSpec.ConfigValue<List<? extends String>> LOG_RESULT_VALUES;
    private static final ForgeConfigSpec.ConfigValue<List<? extends String>> LOG_FLAG_TAGS;
    private static final ForgeConfigSpec.ConfigValue<List<? extends String>> LOG_FLAGS;

    static {
        final ForgeConfigSpec.Builder BUILDER = new ForgeConfigSpec.Builder();

        BUILDER.push("YetAnotherWorldProtector logging configuration").build();

        FLAG_CHECK_LOG = BUILDER.comment("Enable logging of flag checks.")
                .define("log_flag_check", false);

        FLAG_RESULT_LOG = BUILDER.comment("Enable logging of flag check results.")
                .define("log_flag_result", false);

        LOG_EMPTY_RESULTS = BUILDER.comment("Enable logging of empty (without responsible region) flag check results.")
                .define("log_empty_results", false);

        LOG_RESULT_VALUES = BUILDER.comment("List of flags result states which shall be logged. By default only denied results will be logged.\n Valid FlagStates are: allowed and denied")
                .defineListAllowEmpty(List.of("log_result_values"), () -> Collections.singletonList(FlagState.DENIED.name), LoggingConfig::isValidFlagState);

        LOG_FLAG_TAGS = BUILDER.comment("List of flag tags which shall be logged.\nValid tags are: player, beneficial, block, entity, item, environment, protection, high-frequency and * (for all).")
                .defineListAllowEmpty(List.of("log_flag_tags"), () -> Collections.singletonList(FlagTagRegister.PLAYER.tagRl().toString()), LoggingConfig::isValidTag);

        LOG_FLAGS = BUILDER.comment("List of flags which shall be logged.")
                .defineListAllowEmpty(List.of("log_flags"), () -> Arrays.asList(RegionFlag.BREAK_BLOCKS.name, RegionFlag.PLACE_BLOCKS.name), LoggingConfig::isValidFlag);

        // DETAILED_PLAYER_FLAG_LOG = BUILDER.comment("Enable logging of detailed flag checks for player related flags.").define("log_detailed_player_flags", false);

        BUILDER.pop();
        CONFIG_SPEC = BUILDER.build();

    }

    public static Set<String> getFlagTags() {
        return LOG_FLAG_TAGS.get().stream()
                .filter(Objects::nonNull)
                .map(String::toString)
                .collect(Collectors.toSet());
    }

    public static Set<String> getFlagsToLog() {
        return LOG_FLAGS.get().stream()
                .filter(Objects::nonNull)
                .map(String::toString)
                .collect(Collectors.toSet());
    }

    public static Set<String> getResultValuesToLog() {
        return LOG_RESULT_VALUES.get().stream()
                .filter(Objects::nonNull)
                .map(String::toString)
                .collect(Collectors.toSet());
    }

    private static boolean isValidTag(Object entity) {
        if (entity instanceof String str) {
            try {
                if (str.equals("*")) {
                    return true;
                }
                FlagTag tag = FlagTagRegister.from(ResourceLocation.tryParse(str));
                // if no exception was thrown, it's a valid tag
                return tag != null;
            } catch (IllegalArgumentException e) {
                LOGGING_CONFIG_LOGGER.warn("Invalid flag category supplied for 'log_flag_categories': {}", entity);
                return false;
            }
        }
        return false;
    }

    public static boolean isValidFlag(Object flag) {
        if (flag instanceof String) {
            boolean contains = RegionFlag.contains((String) flag);
            if (!contains) {
                LOGGING_CONFIG_LOGGER.warn("Invalid flag supplied for 'log_flags': {}", flag);
            }
            return contains;
        }
        LOGGING_CONFIG_LOGGER.warn("Invalid flag supplied for 'log_flags': {}", flag);
        return false;
    }

    public static boolean isValidFlagState(Object flagState) {
        if (flagState instanceof String) {
            boolean contains = FlagState.validLoggingStates((String) flagState);
            if (!contains) {
                LOGGING_CONFIG_LOGGER.warn("Invalid FlagState supplied for 'log_result_values': {}", flagState);
            }
            return contains;
        }
        LOGGING_CONFIG_LOGGER.warn("Invalid FlagState supplied for 'log_result_values': {}", flagState);
        return false;
    }

    public static boolean shouldLogFlagChecks() {
        return FLAG_CHECK_LOG.get();
    }

    public static boolean shouldLogFlagCheckResults() {
        return FLAG_RESULT_LOG.get();
    }

    public static boolean shouldLogEmptyResults() {
        return LOG_EMPTY_RESULTS.get();
    }

    /*
    public static boolean shouldLogDetailedPlayerFlags() {
        return DETAILED_PLAYER_FLAG_LOG.get();
    }
    */

    public static boolean logCheck(ForgeFlagCheckEvent check) {
        boolean matchesFlagOrCategory = (flagMatchesCategory(check) || matchesFlag(check));
        if (matchesFlagOrCategory) {
            LOGGING_CONFIG_LOGGER.info("[Check] {}, at {}, in '{}', Player={}, Id={}",
                    check.getRegionFlag().name,
                    AreaUtil.blockPosStr(check.getTarget()),
                    check.getDimension().location().toString(),
                    check.getPlayer() == null ? "n/a" : check.getPlayer().getDisplayName().getString(),
                    check.getId());
        }
        return true;
    }

    public static ForgeFlagCheckResult logResult(ForgeFlagCheckResult result) {
        ForgeFlagCheckEvent check = result.getFlagCheck();
        boolean matchesFlagOrCategory = (flagMatchesCategory(check) || matchesFlag(check));
        if (matchesFlagOrCategory && matchesResult(result)) {
            if (result.getResponsible() == null || result.getFlag() == null) {
                // semantically equals to result.getFlagState() == FlagState.UNDEFINED
                if (shouldLogEmptyResults()) {
                    LOGGING_CONFIG_LOGGER.info("[Result] No region for check with Id={}", check.getId());
                }
            } else {
                if (result.getResponsible().getRegionType() != RegionType.LOCAL) {
                    IFlag flag = result.getFlag();
                    LOGGING_CONFIG_LOGGER.info("[Result] {} ({}), Region='{}', Id={}",
                            flag.getName(),
                            result.getFlagState().name,
                            result.getResponsible().getName(),
                            result.getFlagCheck().getId());
                } else {
                    IFlag flag = result.getFlag();
                    LOGGING_CONFIG_LOGGER.info("[Result] {} ({}), Region='{}', in '{}', Id={}",
                            flag.getName(),
                            result.getFlagState().name,
                            result.getResponsible().getName(),
                            result.getResponsible().getDim().location().toString(),
                            result.getFlagCheck().getId());
                }
            }
        }
        return result;
    }

    public static boolean matchesResult(ForgeFlagCheckResult result) {
        return LoggingConfig.getResultValuesToLog().contains(result.getFlagState().name);
    }

    public static boolean flagMatchesCategory(ForgeFlagCheckEvent check) {
        return RegionFlag.matchesCategory(check.getRegionFlag(), getFlagTags());
    }

    public static boolean matchesFlag(ForgeFlagCheckEvent check) {
        return LoggingConfig.getFlagsToLog().contains(check.getRegionFlag().name);
    }
}