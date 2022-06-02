package de.z0rdak.regionshield.commands;

import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.arguments.IntegerArgumentType;
import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.builder.RequiredArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.regionshield.core.region.AbstractMarkableRegion;
import de.z0rdak.regionshield.core.region.IMarkableRegion;
import de.z0rdak.regionshield.core.stick.MarkerStick;
import de.z0rdak.regionshield.managers.data.RegionDataManager;
import de.z0rdak.regionshield.util.MessageUtil;
import de.z0rdak.regionshield.util.RegionUtil;
import de.z0rdak.regionshield.util.StickType;
import de.z0rdak.regionshield.util.StickUtil;
import net.minecraft.command.CommandSource;
import net.minecraft.command.Commands;
import net.minecraft.command.arguments.DimensionArgument;
import net.minecraft.command.arguments.EntityArgument;
import net.minecraft.command.arguments.EntitySelector;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.RegistryKey;
import net.minecraft.util.ResourceLocation;
import net.minecraft.world.World;

import java.util.ArrayList;
import java.util.Collection;

import static de.z0rdak.regionshield.commands.CommandUtils.*;
import static de.z0rdak.regionshield.commands.CommandUtils.buildLiteralFor;
import static net.minecraft.command.ISuggestionProvider.suggest;

public class RegionCommands {

    public static final LiteralArgumentBuilder<CommandSource> REGION_COMMAND = registerRegionCommands();
    public static final LiteralArgumentBuilder<CommandSource> REGIONS_COMMAND = registerRegionsCommands();

    private RegionCommands() {
    }



    private static String getRegionNameArgument(CommandContext<CommandSource> ctx) {
        return StringArgumentType.getString(ctx, CommandConstants.REGION.toString());
    }



    private static ServerPlayerEntity getPlayerArgument(CommandContext<CommandSource> ctx) throws CommandSyntaxException {
        return EntityArgument.getPlayer(ctx, CommandConstants.PLAYER.toString());
    }

    private static ServerPlayerEntity getOwnerArgument(CommandContext<CommandSource> ctx) throws CommandSyntaxException {
        return EntityArgument.getPlayer(ctx, CommandConstants.OWNER.toString());
    }

    private static boolean getActivateArgument(CommandContext<CommandSource> ctx) {
        return BoolArgumentType.getBool(ctx, CommandConstants.ACTIVATE.toString());
    }

    private static boolean getEnableArgument(CommandContext<CommandSource> ctx) {
        return BoolArgumentType.getBool(ctx, CommandConstants.ENABLE.toString());
    }

    private static int getPriorityArgument(CommandContext<CommandSource> ctx) {
        return IntegerArgumentType.getInteger(ctx, CommandConstants.PRIORITY.toString());
    }

    public static LiteralArgumentBuilder<CommandSource> registerRegionsCommands(){
        LiteralArgumentBuilder<CommandSource> regionsLiteral = buildLiteralFor(CommandConstants.REGIONS);
        RequiredArgumentBuilder<CommandSource, ResourceLocation> dimensionArgument = Commands.argument(CommandConstants.DIMENSION.toString(), DimensionArgument.dimension());
        RequiredArgumentBuilder<CommandSource, Boolean> activateArgument = Commands.argument(CommandConstants.ACTIVATE.toString(), BoolArgumentType.bool());

        return regionsLiteral
                .then(Commands.literal(CommandConstants.ACTIVATE.toString())
                        .then(activateArgument
                                // set active state of given region in current dimension
                                .executes(ctx -> setActiveStates(ctx.getSource(), ctx.getSource().getLevel().dimension(), getActivateArgument(ctx))))
                        .then(dimensionArgument
                                // set active state of given region in given dimension
                                .executes(ctx -> setActiveStates(ctx.getSource(), getDimensionFromArgument(ctx), getActivateArgument(ctx)))))
                .then(Commands.literal(CommandConstants.REMOVE.toString())
                        .executes(ctx -> removeRegions(ctx.getSource(), ctx.getSource().getLevel().dimension()))
                        .then(dimensionArgument
                                .executes(ctx -> removeRegions(ctx.getSource(), getDimensionFromArgument(ctx)))));
    }

    // IDEA: Expand command for all area types
    // circle - expand radius
    // ...
    public static LiteralArgumentBuilder<CommandSource> registerRegionCommands() {
        LiteralArgumentBuilder<CommandSource> helpLiteral = buildLiteralFor(CommandConstants.HELP);
        RequiredArgumentBuilder<CommandSource, ResourceLocation> dimensionArgument = Commands.argument(CommandConstants.DIMENSION.toString(), DimensionArgument.dimension());
        LiteralArgumentBuilder<CommandSource> listLiteral = buildLiteralFor(CommandConstants.LIST);
        LiteralArgumentBuilder<CommandSource> infoLiteral = buildLiteralFor(CommandConstants.INFO);
        LiteralArgumentBuilder<CommandSource> createLiteral = buildLiteralFor(CommandConstants.CREATE);
        LiteralArgumentBuilder<CommandSource> regionLiteral = buildLiteralFor(CommandConstants.REGION);
        RequiredArgumentBuilder<CommandSource, String> regionNameArgument = Commands.argument(CommandConstants.REGION.toString(), StringArgumentType.string());
        RequiredArgumentBuilder<CommandSource, EntitySelector> ownerArgument =  Commands.argument(CommandConstants.OWNER.toString(), EntityArgument.player());
        RequiredArgumentBuilder<CommandSource, Boolean> activateArgument = Commands.argument(CommandConstants.ACTIVATE.toString(), BoolArgumentType.bool());
        RequiredArgumentBuilder<CommandSource, Integer> priorityArgument = Commands.argument(CommandConstants.PRIORITY.toString(), IntegerArgumentType.integer(1, Integer.MAX_VALUE));

        LiteralArgumentBuilder<CommandSource> createRegionCommand = createLiteral
                .then(regionNameArgument
                        .executes(ctx -> createRegion(ctx.getSource(), getRegionNameArgument(ctx),  ctx.getSource().getLevel().dimension()))
                        .then(dimensionArgument
                                .suggests((ctx, builder) -> suggest(getQuotedDimensionList(), builder))
                                .executes(ctx -> createRegion(ctx.getSource(), getRegionNameArgument(ctx), getDimensionFromArgument(ctx))))
                        .then(ownerArgument
                                .then(dimensionArgument
                                        .executes(ctx -> createRegion(ctx.getSource(), getRegionNameArgument(ctx), getDimensionFromArgument(ctx), getOwnerArgument(ctx))))));

        LiteralArgumentBuilder<CommandSource> setActiveStateCommand = Commands.literal(CommandConstants.ACTIVATE.toString())
                .then(regionNameArgument
                        .suggests((ctx, builder) -> suggest(RegionDataManager.get().getAllRegionNames(), builder))
                        .then(activateArgument
                                // set active state of given region in current dimension
                                .executes(ctx -> setActiveState(ctx.getSource(), getRegionNameArgument(ctx), ctx.getSource().getLevel().dimension(), getActivateArgument(ctx))))
                        .then(dimensionArgument
                                // set active state of given region in given dimension
                                .executes(ctx -> setActiveState(ctx.getSource(), getRegionNameArgument(ctx), getDimensionFromArgument(ctx), getActivateArgument(ctx)))));

        return regionLiteral
                .executes(ctx -> promptHelp(ctx.getSource()))
                .then(helpLiteral
                        .executes(ctx -> promptHelp(ctx.getSource())))
                .then(listLiteral
                        .executes(ctx -> promptRegionList(ctx.getSource()))
                        .then(dimensionArgument
                                .suggests((ctx, builder) -> suggest(getQuotedDimensionList(), builder))
                                .executes(ctx -> promptRegionListForDim(ctx.getSource(), getDimensionFromArgument(ctx)))))
                .then(infoLiteral
                        .executes(ctx -> listRegionsAround(ctx.getSource()))
                        .then(regionNameArgument
                                .suggests((ctx, builder) -> suggest(RegionDataManager.get().getAllRegionNames(), builder))
                                .executes(ctx -> info(ctx.getSource(), getRegionNameArgument(ctx)))))
                .then(createRegionCommand)
                // UPDATE AREA
                .then(Commands.literal(CommandConstants.UPDATE.toString())
                        .then(regionNameArgument
                                .suggests((ctx, builder) -> suggest(RegionDataManager.get().getAllRegionNames(), builder))
                                .executes(ctx -> updateRegion(ctx.getSource(), getRegionNameArgument(ctx)))))
                // REMOVE REGION
                .then(Commands.literal(CommandConstants.REMOVE.toString())
                        .then(regionNameArgument
                                .suggests((ctx, builder) -> suggest(RegionDataManager.get().getAllRegionNames(), builder))
                                .executes(ctx -> removeRegion(ctx.getSource(), getRegionNameArgument(ctx), ctx.getSource().getLevel().dimension())))
                        .then(dimensionArgument
                                .executes(ctx -> removeRegion(ctx.getSource(), getRegionNameArgument(ctx), getDimensionFromArgument(ctx)))))
                // TP TO REGION
                .then(Commands.literal(CommandConstants.TELEPORT.toString())
                        .then(regionNameArgument
                                .suggests((ctx, builder) -> suggest(RegionDataManager.get().getAllRegionNames(), builder))
                                .executes(ctx -> teleport(ctx.getSource(), getRegionNameArgument(ctx)))))

                .then(setActiveStateCommand)
                // ALERT
                .then(Commands.literal(CommandConstants.ALERT.toString())
                        .then(regionNameArgument
                                .suggests((ctx, builder) -> suggest(RegionDataManager.get().getAllRegionNames(), builder))
                                .then(Commands.argument(CommandConstants.ENABLE.toString(), BoolArgumentType.bool())
                                        .executes(ctx -> setAlertState(ctx.getSource(), getRegionNameArgument(ctx), getEnableArgument(ctx))))))
                // PRIORITY
                .then(Commands.literal(CommandConstants.PRIORITY.toString())
                        .then(regionNameArgument
                                .suggests((ctx, builder) -> suggest(RegionDataManager.get().getAllRegionNames(), builder))
                                .then(priorityArgument
                                        .executes(ctx -> setPriority(ctx.getSource(), getRegionNameArgument(ctx), getPriorityArgument(ctx))))));
    }

    private static int removeRegion(CommandSource source, String regionName, RegistryKey<World> dim) {
        return 0;
    }

    private static int info(CommandSource source, String regionName) {
        try {
            RegionUtil.promptInteractiveRegionInfo(source.getPlayerOrException(), regionName);
        } catch (CommandSyntaxException e) {
            e.printStackTrace();
        }
        return 0;
    }

    private static int setActiveStates(CommandSource source, RegistryKey<World> dim, boolean activate) {
        try {
            // TODO: Handle errors and give feedback to player
            PlayerEntity player = source.getPlayerOrException();
            Collection<IMarkableRegion> regionsToProcess = RegionDataManager.get().getRegionsFor(dim);
            RegionDataManager.get().setActiveState(regionsToProcess, activate);
        } catch (CommandSyntaxException e) {
            e.printStackTrace();
        }
        return 0;
    }

    private static int setActiveState(CommandSource source, String regionName, RegistryKey<World> dim, boolean activate) {
        try {
            // TODO: Handle errors and give feedback to player
            PlayerEntity player = source.getPlayerOrException();
            Collection<IMarkableRegion> regionsToProcess = new ArrayList<>();
            IMarkableRegion region = RegionDataManager.get().getRegionIn(regionName, dim);
            if (region != null) {
                regionsToProcess.add(region);
            }
            RegionDataManager.get().setActiveState(regionsToProcess, activate);
        } catch (CommandSyntaxException e) {
            e.printStackTrace();
        }
        return 0;
    }

    private static int setAlertState(CommandSource source, String regionName, boolean mute) {
        try {
            RegionUtil.muteRegion(regionName, source.getPlayerOrException(), mute);
        } catch (CommandSyntaxException e) {
            e.printStackTrace();
        }
        return 0;
    }

    private static int promptHelp(CommandSource source) {
        try {
            MessageUtil.promptRegionHelp(source.getPlayerOrException());
        } catch (CommandSyntaxException e) {
            e.printStackTrace();
        }
        return 0;
    }

    private static int promptRegionList(CommandSource source) {
        try {
            promptRegionListForDim(source, source.getPlayerOrException().getCommandSenderWorld().dimension());
        } catch (CommandSyntaxException e) {
            e.printStackTrace();
        }
        return 0;
    }

    private static int promptRegionListForDim(CommandSource source, RegistryKey<World> dim) {
        try {
            RegionUtil.giveRegionListForDim(source.getPlayerOrException(), dim.location().toString());
        } catch (CommandSyntaxException e) {
            e.printStackTrace();
        }
        return 0;
    }

    private static int createRegion(CommandSource source, String regionName, RegistryKey<World> dim) {
        try {
            createRegion(source, regionName, dim, source.getPlayerOrException());
        } catch (CommandSyntaxException e) {
            e.printStackTrace();
        }
        return 0;
    }

    private static int createRegion(CommandSource source, String regionName, RegistryKey<World> dim, PlayerEntity owner) {
        try {
            PlayerEntity player = source.getPlayerOrException();
            ItemStack maybeStick = player.getMainHandItem();
            // TODO: create a method which trhows exception on trying to get stick
            if (StickUtil.isVanillaStick(maybeStick)) {
                StickType stickType = StickUtil.getStickType(maybeStick);
                if (stickType == StickType.MARKER) {
                    CompoundNBT stickNBT = StickUtil.getStickNBT(maybeStick);
                    if (stickNBT != null) {
                        AbstractMarkableRegion region = RegionUtil.regionFrom(source.getPlayerOrException(), new MarkerStick(stickNBT), regionName);
                        // TODO
                    }
                }
            }
        } catch (CommandSyntaxException e) {
            handleCommonWithoutPlayer(e);
        }
        return 0;
    }

    // assumption: regions are only updated with the region marker when in the same dimension
    private static int updateRegion(CommandSource source, String regionName) {
        try {
            PlayerEntity player = source.getPlayerOrException();
            ItemStack maybeStick = player.getMainHandItem();
            if (StickUtil.isVanillaStick(maybeStick)) {
                StickType stickType = StickUtil.getStickType(maybeStick);
                if (stickType == StickType.MARKER) {
                    CompoundNBT stickNBT = StickUtil.getStickNBT(maybeStick);
                    if (stickNBT != null) {
                        // TODO: get region and update area

                    }
                }
            }
            RegionUtil.update(regionName, source.getPlayerOrException(), source.getPlayerOrException().getMainHandItem());
        } catch (CommandSyntaxException e) {
            e.printStackTrace();
        }
        return 0;
    }

    private static void handleCommonWithoutPlayer(CommandSyntaxException e) {
        e.printStackTrace();
    }

    private static int removeRegions(CommandSource source, RegistryKey<World> dim) {
        try {
            //
            PlayerEntity player = source.getPlayerOrException();
        } catch (CommandSyntaxException e) {
            e.printStackTrace();
        }
        return 0;
    }

    private static int listRegionsAround(CommandSource source) {
        try {
            RegionUtil.listRegionsAroundPlayer(source.getPlayerOrException());
        } catch (CommandSyntaxException e) {
            e.printStackTrace();
        }
        return 0;
    }

    private static int teleport(CommandSource source, String regionName) {
        try {
            RegionUtil.teleportToRegion(regionName, source.getPlayerOrException(), source);
        } catch (CommandSyntaxException e) {
            e.printStackTrace();
        }
        return 0;
    }

    private static int setPriority(CommandSource source, String region, int priority) {
        try {
            RegionUtil.setRegionPriority(region, priority, source.getPlayerOrException());
        } catch (CommandSyntaxException e) {
            e.printStackTrace();
        }
        return 0;
    }
}
