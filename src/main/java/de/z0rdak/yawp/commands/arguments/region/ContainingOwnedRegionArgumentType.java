package de.z0rdak.yawp.commands.arguments.region;

import com.mojang.brigadier.StringReader;
import com.mojang.brigadier.arguments.ArgumentType;
import com.mojang.brigadier.arguments.IntegerArgumentType;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import com.mojang.brigadier.exceptions.DynamicCommandExceptionType;
import com.mojang.brigadier.exceptions.SimpleCommandExceptionType;
import com.mojang.brigadier.suggestion.Suggestions;
import com.mojang.brigadier.suggestion.SuggestionsBuilder;
import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.commands.CommandSourceType;
import de.z0rdak.yawp.config.server.CommandPermissionConfig;
import de.z0rdak.yawp.core.area.AreaType;
import de.z0rdak.yawp.core.area.CuboidArea;
import de.z0rdak.yawp.core.area.IMarkableArea;
import de.z0rdak.yawp.core.area.SphereArea;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.core.stick.MarkerStick;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import de.z0rdak.yawp.util.LocalRegions;
import de.z0rdak.yawp.util.StickUtil;
import net.minecraft.command.CommandSource;
import net.minecraft.command.ISuggestionProvider;
import net.minecraft.command.arguments.BlockPosArgument;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.text.TranslationTextComponent;

import javax.annotation.Nullable;
import java.util.Collection;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static de.z0rdak.yawp.commands.CommandConstants.*;
import static de.z0rdak.yawp.commands.MarkerCommands.fromMarkedBlocks;
import static de.z0rdak.yawp.util.MessageSender.sendCmdFeedback;

public class ContainingOwnedRegionArgumentType implements ArgumentType<String> {

    public static final Pattern VALID_NAME_PATTERN = Pattern.compile("^[A-Za-z]+[A-Za-z\\d\\-]+[A-Za-z\\d]+$");
    private static final Collection<String> EXAMPLES = Stream.of(new String[]{"spawn", "arena4pvp", "shop", "nether-hub"})
            .collect(Collectors.toSet());
    private static final SimpleCommandExceptionType ERROR_AREA_INVALID = new SimpleCommandExceptionType(new TranslationTextComponent("cli.arg.region.parse.invalid"));
    private static final DynamicCommandExceptionType ERROR_INVALID_VALUE = new DynamicCommandExceptionType(
            regionName -> new TranslationTextComponent("cli.arg.region.invalid", regionName)
    );
    private static final DynamicCommandExceptionType ERROR_INVALID_PARENT = new DynamicCommandExceptionType(
            regionName -> new TranslationTextComponent("cli.arg.region.owned.invalid", regionName)
    );


    /**
     * Using this as an actual argument does not work on a server-side only mod,
     * because it needs to be registered in the corresponding registry.
     */
    public static ContainingOwnedRegionArgumentType owningRegions() {
        return new ContainingOwnedRegionArgumentType();
    }

    public static IMarkableRegion getRegion(CommandContext<CommandSource> context, String argName) throws CommandSyntaxException {
        String containingRegionName = context.getArgument(argName, String.class);
        String containedRegionName = context.getArgument(NAME.toString(), String.class);
        DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(context.getSource().getLevel().dimension());
        IMarkableRegion parent = dimCache.getRegion(containingRegionName);

        IMarkableArea markedArea = getMarkableArea(context);
        if (markedArea == null) {
            throw new IllegalArgumentException("Could not get marked blocks from command");
        }
        if (parent == null) {
            throw ERROR_INVALID_VALUE.create(containingRegionName);
        }
        boolean hasPermissionForParent = CommandPermissionConfig.hasConfigPermission(context.getSource(), CommandSourceType.of(context.getSource()));
        boolean containsChild = parent.getArea().containsOther(markedArea);
        if (hasPermissionForParent && containsChild) {
            return parent;
        } else {
            if (!hasPermissionForParent) {
                sendCmdFeedback(context.getSource(), new TranslationTextComponent("cli.arg.region.owned.invalid.permission", containingRegionName, containedRegionName));
            }
            if (!containsChild) {
                sendCmdFeedback(context.getSource(), new TranslationTextComponent("cli.arg.region.owned.invalid.containment", containingRegionName, containedRegionName));
            }
            throw ERROR_INVALID_PARENT.create(containingRegionName);
        }
    }

    public static IMarkableRegion getRegionWithMarker(CommandContext<CommandSource> context, String argName) throws CommandSyntaxException {
        String containingRegionName = context.getArgument(argName, String.class);
        String containedRegionName = context.getArgument(NAME.toString(), String.class);
        DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(context.getSource().getLevel().dimension());
        IMarkableRegion parent = dimCache.getRegion(containingRegionName);

        ServerPlayerEntity player = context.getSource().getPlayerOrException();
        IMarkableRegion markedRegion = fromMarkedBlocks(context, player, containedRegionName);
        if (markedRegion == null) {
            throw new IllegalArgumentException("Could not get marked blocks from command");
        }
        if (parent == null) {
            throw ERROR_INVALID_VALUE.create(containingRegionName);
        }
        boolean hasPermissionForParent = CommandPermissionConfig.hasConfigPermission(context.getSource(), CommandSourceType.of(context.getSource()));
        boolean containsChild = parent.getArea().containsOther(markedRegion.getArea());
        if (hasPermissionForParent && containsChild) {
            return parent;
        } else {
            if (!hasPermissionForParent) {
                sendCmdFeedback(context.getSource(), new TranslationTextComponent("cli.arg.region.owned.invalid.permission", containingRegionName, containedRegionName));
            }
            if (!containsChild) {
                sendCmdFeedback(context.getSource(), new TranslationTextComponent("cli.arg.region.owned.invalid.containment", containingRegionName, containedRegionName));
            }
            throw ERROR_INVALID_PARENT.create(containingRegionName);
        }
    }

    @Override
    public String parse(StringReader reader) throws CommandSyntaxException {
        int i = reader.getCursor();
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

    /**
     * Suggests regions with permission, which are also fully containing the area provided by the marked blocks of the region marker
     */
    public <S> CompletableFuture<Suggestions> listSuggestionsWithMarker(CommandContext<S> context, SuggestionsBuilder builder) {
        if (context.getSource() instanceof CommandSource) {
            CommandSource src = (CommandSource) context.getSource();
            try {
                PlayerEntity player = src.getPlayerOrException();
                ItemStack maybeStick = player.getMainHandItem();
                if (StickUtil.isVanillaStick(maybeStick) && StickUtil.isMarker(maybeStick)) {
                    CompoundNBT stickNBT = StickUtil.getStickNBT(maybeStick);
                    if (stickNBT != null) {
                        MarkerStick marker = new MarkerStick(stickNBT);
                        if (!marker.isValidArea()) {
                            return Suggestions.empty();
                        }
                        IMarkableArea markedArea = StickUtil.getMarkedArea(player.getMainHandItem());
                        LocalRegions.RegionOverlappingInfo overlapping = LocalRegions.getOverlappingWithPermission(markedArea, player);
                        if (!overlapping.hasContaining()) {
                            sendCmdFeedback(src, new TranslationTextComponent("cli.arg.area.owned.no-containment"));
                            return Suggestions.empty();
                        }
                        Set<String> containingRegionName = overlapping.containingRegions.stream().map(IProtectedRegion::getName).collect(Collectors.toSet());
                        return ISuggestionProvider.suggest(containingRegionName, builder);
                    }
                }
                return Suggestions.empty();
            } catch (CommandSyntaxException e) {
                YetAnotherWorldProtector.LOGGER.error(e);
                return Suggestions.empty();
            }
        } else {
            return Suggestions.empty();
        }
    }

    private static <S> @Nullable IMarkableArea getMarkableArea(CommandContext<CommandSource> ctx) throws CommandSyntaxException {
        IMarkableArea markedArea = null;
        AreaType areaType = null;
        if (ctx.getInput().contains(AreaType.CUBOID.areaType)) {
            areaType = AreaType.CUBOID;
        }
        if (ctx.getInput().contains(AreaType.SPHERE.areaType)) {
            areaType = AreaType.SPHERE;
        }
        switch (areaType) {
            case CUBOID:
                BlockPos p1 = BlockPosArgument.getLoadedBlockPos(ctx, POS1.toString());
                BlockPos p2 = BlockPosArgument.getLoadedBlockPos(ctx, POS2.toString());
                markedArea = new CuboidArea(p1, p2);
                break;
            case SPHERE:
                try {
                    BlockPos centerPos = BlockPosArgument.getLoadedBlockPos(ctx, CENTER_POS.toString());
                    BlockPos radiusPos = BlockPosArgument.getLoadedBlockPos(ctx, RADIUS_POS.toString());
                    markedArea = new SphereArea(centerPos, radiusPos);
                } catch (CommandSyntaxException cse) {
                    BlockPos centerPos = BlockPosArgument.getLoadedBlockPos(ctx, CENTER_POS.toString());
                    int radius = IntegerArgumentType.getInteger(ctx, RADIUS.toString());
                    markedArea = new SphereArea(centerPos, radius);
                }
                break;
            default:
                throw new IllegalStateException("Unexpected value: " + areaType);
        }
        return markedArea;
    }

    /**
     * Suggests regions with permission, which are also fully containing the area provided by the create local command
     */
    public <S> CompletableFuture<Suggestions> listSuggestions(CommandContext<S> context, SuggestionsBuilder builder) {
        if (context.getSource() instanceof CommandSource) {
            CommandContext<CommandSource> ctx = (CommandContext<CommandSource>) context;
            CommandSource src = (CommandSource) context.getSource();
            try {
                IMarkableArea markedArea = getMarkableArea(ctx);
                if (markedArea == null) {
                    throw new IllegalArgumentException("Could not get marked blocks from command");
                }
                PlayerEntity player;
                LocalRegions.RegionOverlappingInfo overlapping;
                try {
                    player = src.getPlayerOrException();
                    overlapping = LocalRegions.getOverlappingWithPermission(markedArea, player);
                } catch (CommandSyntaxException e) {
                    overlapping = LocalRegions.getOverlappingRegions(markedArea, src.getLevel().dimension());
                }
                if (!overlapping.hasContaining()) {
                    sendCmdFeedback(src, new TranslationTextComponent("cli.arg.area.owned.no-containment"));
                    return Suggestions.empty();
                }
                Set<String> containingRegionName = overlapping.containingRegions.stream().map(IProtectedRegion::getName).collect(Collectors.toSet());
                return ISuggestionProvider.suggest(containingRegionName, builder);
            } catch (CommandSyntaxException e) {
                return Suggestions.empty();
            }
        } else {
            return Suggestions.empty();
        }
    }

    @Override
    public Collection<String> getExamples() {
        return EXAMPLES;
    }
}
