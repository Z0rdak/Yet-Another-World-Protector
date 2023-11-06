package de.z0rdak.yawp.commands.arguments.region;

import com.mojang.brigadier.StringReader;
import com.mojang.brigadier.arguments.ArgumentType;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.context.ParsedCommandNode;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import com.mojang.brigadier.exceptions.DynamicCommandExceptionType;
import com.mojang.brigadier.exceptions.SimpleCommandExceptionType;
import com.mojang.brigadier.suggestion.Suggestions;
import com.mojang.brigadier.suggestion.SuggestionsBuilder;
import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.commands.CommandConstants;
import de.z0rdak.yawp.commands.arguments.ArgumentUtil;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.core.region.RegionType;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import de.z0rdak.yawp.util.MessageUtil;
import net.minecraft.command.CommandSource;
import net.minecraft.command.ISuggestionProvider;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.util.text.StringTextComponent;
import net.minecraft.util.text.TranslationTextComponent;

import java.util.Collection;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static de.z0rdak.yawp.commands.CommandConstants.FLAG;
import static de.z0rdak.yawp.config.server.CommandPermissionConfig.BASE_CMD;

public class RegionArgumentType implements ArgumentType<String> {

    private static final Collection<String> EXAMPLES = Stream.of(new String[]{"spawn", "arena4pvp", "shop", "nether-hub"})
            .collect(Collectors.toSet());

    private static final SimpleCommandExceptionType ERROR_AREA_INVALID = new SimpleCommandExceptionType(new TranslationTextComponent("cli.arg.region.parse.invalid"));

    private static final DynamicCommandExceptionType ERROR_INVALID_VALUE = new DynamicCommandExceptionType(
            flag -> new TranslationTextComponent("cli.arg.region.invalid", flag)
    );

    public static final Pattern VALID_NAME_PATTERN = Pattern.compile("^[A-Za-z]+[A-Za-z\\d\\-]+[A-Za-z\\d]+$");

    public static <S> RegionType getRegionType(CommandContext<S> context) {
        List<ParsedCommandNode<S>> nodes = context.getNodes();
        if (nodes.size() >= 2) {
            String baseCmd = nodes.get(0).getNode().getName();
            if (baseCmd.equals(BASE_CMD)) {
                String regionTypeLiteral = nodes.get(1).getNode().getName();
                RegionType regionType = RegionType.of(regionTypeLiteral);
                boolean isFlagSubCmd = regionTypeLiteral.equals(FLAG.toString()) && nodes.size() >= 3;
                if (regionType == null && isFlagSubCmd) {
                    String flagRegionTypeLiteral = nodes.get(2).getNode().getName();
                    return RegionType.of(flagRegionTypeLiteral);
                }
            }
        }
        return null;
    }

    public static IMarkableRegion getRegion(CommandContext<CommandSource> context, String argName) throws CommandSyntaxException {
        String regionName = context.getArgument(argName, String.class);
        DimensionRegionCache dimCache = ArgumentUtil.getDimCacheArgument(context);
        if (!dimCache.contains(regionName)) {
            MessageUtil.sendCmdFeedback(context.getSource(), new StringTextComponent("No region with name '" + regionName + "' defined in dim '" + dimCache.dimensionKey().location() + "'"));
            throw ERROR_INVALID_VALUE.create(regionName);
        }
        IMarkableRegion region = dimCache.getRegion(regionName);
        if (region != null) {
            return region;
        } else {
            MessageUtil.sendCmdFeedback(context.getSource(), new StringTextComponent("No regions defined in dim '" + dimCache.dimensionKey().location() + "'"));
            throw ERROR_INVALID_VALUE.create(regionName);
        }
    }

    public static IProtectedRegion getRegion(CommandContext<CommandSource> context, RegionType regionType) throws CommandSyntaxException {
        switch (regionType) {
            case GLOBAL:
                return RegionDataManager.get().getGlobalRegion();
            case DIMENSION: {
                DimensionRegionCache dimCache = ArgumentUtil.getDimCacheArgument(context);
                return dimCache.getDimensionalRegion();
            }
            case LOCAL: {
                DimensionRegionCache dimCache = ArgumentUtil.getDimCacheArgument(context);
                String regionName = context.getArgument(CommandConstants.REGION.toString(), String.class);
                if (!dimCache.contains(regionName)) {
                    MessageUtil.sendCmdFeedback(context.getSource(), new StringTextComponent("No region with name '" + regionName + "' defined in dim '" + dimCache.dimensionKey().location() + "'"));
                    throw ERROR_INVALID_VALUE.create(regionName);
                }
                IMarkableRegion region = dimCache.getRegion(regionName);
                if (region != null) {
                    return region;
                } else {
                    MessageUtil.sendCmdFeedback(context.getSource(), new StringTextComponent("No regions defined in dim '" + dimCache.dimensionKey().location() + "'"));
                    throw ERROR_INVALID_VALUE.create(regionName);
                }
            }
            default:
                throw ERROR_INVALID_VALUE.create("");
        }
    }

    @Override
    public String parse(StringReader reader) throws CommandSyntaxException {
        int i = reader.getCursor();
        // Pattern only matches chars, not the valid name
        while (reader.canRead() && String.valueOf(reader.peek()).matches(Pattern.compile("^[A-Za-z\\d\\-]$").pattern())) {
            reader.skip();
        }
        String s = reader.getString().substring(i, reader.getCursor());

        try {
            boolean isValidName = s.matches(VALID_NAME_PATTERN.pattern());
            if (isValidName) {
                return s;
            } else {
                throw new IllegalArgumentException("Invalid region name supplied");
            }
        } catch (IllegalArgumentException argumentException) {
            reader.setCursor(i);
            YetAnotherWorldProtector.LOGGER.error("Error parsing region name");
            throw ERROR_AREA_INVALID.createWithContext(reader);
        }
    }

    public static IMarkableRegion getSrcRegion(CommandContext<CommandSource> context, String argName) throws CommandSyntaxException {
        String regionName = context.getArgument(argName, String.class);
        DimensionRegionCache dimCache = ArgumentUtil.getSrcDimCacheArgument(context);
        if (!dimCache.contains(regionName)) {
            MessageUtil.sendCmdFeedback(context.getSource(), new StringTextComponent("No region with name '" + regionName + "' defined in dim '" + dimCache.dimensionKey().location() + "'"));
            throw ERROR_INVALID_VALUE.create(regionName);
        }
        IMarkableRegion region = dimCache.getRegion(regionName);
        if (region != null) {
            return region;
        } else {
            MessageUtil.sendCmdFeedback(context.getSource(), new StringTextComponent("No regions defined in dim '" + dimCache.dimensionKey().location() + "'"));
            throw ERROR_INVALID_VALUE.create(regionName);
        }
    }

    @Override
    public Collection<String> getExamples() {
        return EXAMPLES;
    }

    /**
     * Using this as an actual argument does not work on a server-side only mod,
     * because it needs to be registered in the corresponding registry.
     */
    public static RegionArgumentType region() {
        return new RegionArgumentType();
    }

    @SuppressWarnings("unchecked")
    @Override
    public <S> CompletableFuture<Suggestions> listSuggestions(CommandContext<S> context, SuggestionsBuilder builder) {
        if (context.getSource() instanceof CommandSource) {
            CommandSource src = (CommandSource) context.getSource();
            try {
                DimensionRegionCache dimCache = ArgumentUtil.getDimCacheArgument((CommandContext<CommandSource>) context);
                Collection<String> regionNames = dimCache.getRegionNames();
                if (regionNames.isEmpty()) {
                    MessageUtil.sendCmdFeedback(src, new StringTextComponent("No regions defined in dim '" + dimCache.dimensionKey().location() + "'"));
                    return Suggestions.empty();
                }
                return ISuggestionProvider.suggest(regionNames, builder);
            } catch (CommandSyntaxException e) {
                return Suggestions.empty();
            }
        } else {
            return Suggestions.empty();
        }
    }

    @SuppressWarnings("unchecked")
    public <S> CompletableFuture<Suggestions> listSrcRegions(CommandContext<S> context, SuggestionsBuilder builder) {
        if (context.getSource() instanceof CommandSource) {
            CommandSource src = (CommandSource) context.getSource();
            try {
                DimensionRegionCache dimCache = ArgumentUtil.getSrcDimCacheArgument((CommandContext<CommandSource>) context);
                Collection<String> regionNames = dimCache.getRegionNames();
                if (regionNames.isEmpty()) {
                    MessageUtil.sendCmdFeedback(src, new StringTextComponent("No regions defined in dim '" + dimCache.dimensionKey().location() + "'"));
                    return Suggestions.empty();
                }
                return ISuggestionProvider.suggest(regionNames, builder);
            } catch (CommandSyntaxException e) {
                return Suggestions.empty();
            }
        } else {
            return Suggestions.empty();
        }
    }

    public static IMarkableRegion getRegionInPlayerDim(CommandContext<CommandSource> context, String argName) throws CommandSyntaxException {
        String regionName = context.getArgument(argName, String.class);
        PlayerEntity player = context.getSource().getPlayerOrException();
        DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(player.level.dimension());
        IMarkableRegion region = dimCache.getRegion(regionName);
        if (region != null) {
            return region;
        } else {
            MessageUtil.sendCmdFeedback(context.getSource(), new StringTextComponent("No regions defined in dim '" + dimCache.dimensionKey().location() + "'"));
            throw ERROR_INVALID_VALUE.create(regionName);
        }
    }
}
