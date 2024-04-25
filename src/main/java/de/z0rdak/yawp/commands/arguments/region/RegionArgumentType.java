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
import de.z0rdak.yawp.config.server.CommandPermissionConfig;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.core.region.RegionType;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
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
import static de.z0rdak.yawp.util.MessageSender.sendCmdFeedback;

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

                if (isFlagSubCmd && regionType == null) {
                    String flagRegionTypeLiteral = nodes.get(2).getNode().getName();
                    return RegionType.of(flagRegionTypeLiteral);
                }
                if (!isFlagSubCmd && regionType != null) {
                    return regionType;
                }
            }
        }
        return null;
    }

    public static IMarkableRegion getRegion(CommandContext<CommandSource> context, String argName) throws CommandSyntaxException {
        String regionName = context.getArgument(argName, String.class);
        DimensionRegionCache dimCache = ArgumentUtil.getDimCacheArgument(context);
        if (!dimCache.contains(regionName)) {
            sendCmdFeedback(context.getSource(), new StringTextComponent("No region with name '" + regionName + "' defined in dim '" + dimCache.dimensionKey().location() + "'"));
            throw ERROR_INVALID_VALUE.create(regionName);
        }
        IMarkableRegion region = dimCache.getRegion(regionName);
        if (region != null) {
            return region;
        } else {
            sendCmdFeedback(context.getSource(), new StringTextComponent("No regions defined in dim '" + dimCache.dimensionKey().location() + "'"));
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
                String regionName = context.getArgument(CommandConstants.LOCAL.toString(), String.class);
                if (!dimCache.contains(regionName)) {
                    sendCmdFeedback(context.getSource(), new StringTextComponent("No region with name '" + regionName + "' defined in dim '" + dimCache.dimensionKey().location() + "'"));
                    throw ERROR_INVALID_VALUE.create(regionName);
                }
                IMarkableRegion region = dimCache.getRegion(regionName);
                if (region != null) {
                    return region;
                } else {
                    sendCmdFeedback(context.getSource(), new StringTextComponent("No regions defined in dim '" + dimCache.dimensionKey().location() + "'"));
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

    public static IProtectedRegion getTargetRegion(CommandContext<CommandSource> context, String argName) throws CommandSyntaxException {
        String regionName = context.getArgument(argName, String.class);
        DimensionRegionCache dimCache = ArgumentUtil.getTargetDimRegionArgument(context);
        if (!dimCache.contains(regionName)) {
            sendCmdFeedback(context.getSource(), new StringTextComponent("No region with name '" + regionName + "' defined in dim '" + dimCache.dimensionKey().location() + "'"));
            throw ERROR_INVALID_VALUE.create(regionName);
        }
        IMarkableRegion region = dimCache.getRegion(regionName);
        if (region != null) {
            return region;
        } else {
            sendCmdFeedback(context.getSource(), new StringTextComponent("No regions defined in dim '" + dimCache.dimensionKey().location() + "'"));
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
            DimensionRegionCache dimCache = ArgumentUtil.getDimCacheArgument((CommandContext<CommandSource>) context);
            return suggestRegionsForOwner(builder, src, dimCache);
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
            sendCmdFeedback(context.getSource(), new StringTextComponent("No suitable region as parent found in '" + dimCache.dimensionKey().location() + "'"));
            throw ERROR_INVALID_VALUE.create(regionName);
        }
    }

    @SuppressWarnings("unchecked")
    public <S> CompletableFuture<Suggestions> listRegionsInTargetDim(CommandContext<S> context, SuggestionsBuilder builder) {
        if (context.getSource() instanceof CommandSource) {
            CommandSource src = (CommandSource) context.getSource();
            try {
                DimensionRegionCache dimCache = ArgumentUtil.getTargetDimRegionArgument((CommandContext<CommandSource>) context);
                return suggestRegionsForOwner(builder, src, dimCache);
            } catch (CommandSyntaxException e) {
                return Suggestions.empty();
            }
        } else {
            return Suggestions.empty();
        }
    }

    private CompletableFuture<Suggestions> suggestRegionsForOwner(SuggestionsBuilder builder, CommandSource src, DimensionRegionCache dimCache) {
        Collection<IMarkableRegion> regions = dimCache.getRegions();
        boolean hasPermission = CommandPermissionConfig.hasCmdPermission(src);
        if (hasPermission) {
            Collection<String> regionNames = dimCache.getRegions().stream().map(IProtectedRegion::getName).collect(Collectors.toSet());
            if (regionNames.isEmpty()) {
                sendCmdFeedback(src, new StringTextComponent("No regions defined in dim '" + dimCache.dimensionKey().location() + "'"));
                return Suggestions.empty();
            } else {
                return ISuggestionProvider.suggest(regionNames, builder);
            }
        } else {
            if (src.getEntity() instanceof PlayerEntity) {
                PlayerEntity player = (PlayerEntity) src.getEntity();
                regions = regions.stream()
                        .filter(region -> CommandPermissionConfig.hasRegionPermission(region, player))
                        .collect(Collectors.toList());
                Collection<String> regionNames = regions.stream().map(IProtectedRegion::getName).collect(Collectors.toSet());
                if (regionNames.isEmpty()) {
                    sendCmdFeedback(src, new StringTextComponent("No regions defined in dim '" + dimCache.dimensionKey().location() + "'"));
                    return Suggestions.empty();
                }
                return ISuggestionProvider.suggest(regionNames, builder);
            }
        }
        return Suggestions.empty();
    }
}
