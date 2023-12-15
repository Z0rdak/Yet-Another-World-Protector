package de.z0rdak.yawp.commands;

import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.arguments.IntegerArgumentType;
import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.commands.arguments.ArgumentUtil;
import de.z0rdak.yawp.commands.arguments.region.AddRegionChildArgumentType;
import de.z0rdak.yawp.commands.arguments.region.RegionArgumentType;
import de.z0rdak.yawp.commands.arguments.region.RemoveRegionChildArgumentType;
import de.z0rdak.yawp.config.server.RegionConfig;
import de.z0rdak.yawp.core.area.AreaType;
import de.z0rdak.yawp.core.area.CuboidArea;
import de.z0rdak.yawp.core.area.IMarkableArea;
import de.z0rdak.yawp.core.flag.BooleanFlag;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.group.GroupType;
import de.z0rdak.yawp.core.region.*;
import de.z0rdak.yawp.core.stick.AbstractStick;
import de.z0rdak.yawp.core.stick.MarkerStick;
import de.z0rdak.yawp.handler.flags.HandlerUtil;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import de.z0rdak.yawp.util.LocalRegions;
import de.z0rdak.yawp.util.StickException;
import de.z0rdak.yawp.util.StickType;
import de.z0rdak.yawp.util.StickUtil;
import net.minecraft.ChatFormatting;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.SharedSuggestionProvider;
import net.minecraft.commands.arguments.DimensionArgument;
import net.minecraft.commands.arguments.EntityArgument;
import net.minecraft.commands.arguments.TeamArgument;
import net.minecraft.commands.arguments.coordinates.BlockPosArgument;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.MutableComponent;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.network.chat.TranslatableComponent;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.ExperienceOrb;
import net.minecraft.world.entity.animal.AbstractGolem;
import net.minecraft.world.entity.monster.Slime;
import net.minecraft.world.entity.npc.WanderingTrader;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.entity.EntityTypeTest;
import net.minecraft.world.scores.PlayerTeam;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.commands.CommandConstants.*;
import static de.z0rdak.yawp.commands.DimensionCommands.checkValidRegionName;
import static de.z0rdak.yawp.commands.arguments.ArgumentUtil.*;
import static de.z0rdak.yawp.util.MessageUtil.*;
import static net.minecraft.ChatFormatting.RESET;

public class RegionCommands {

    private final static int MIN_BUILD_LIMIT = 0;
    private final static int MAX_BUILD_LIMIT = 255;

    private RegionCommands() {
    }

    public static LiteralArgumentBuilder<CommandSourceStack> build() {
        return literal(REGION)
                .then(Commands.argument(DIM.toString(), DimensionArgument.dimension())
                        .then(Commands.argument(REGION.toString(), StringArgumentType.word())
                                .suggests((ctx, builder) -> RegionArgumentType.region().listSuggestions(ctx, builder))
                                .executes(ctx -> CommandUtil.promptRegionInfo(ctx, getRegionArgument(ctx)))
                                .then(literal(INFO)
                                        .executes(ctx -> CommandUtil.promptRegionInfo(ctx, getRegionArgument(ctx))))
                                .then(CommandUtil.buildClearSubCommand(ArgumentUtil::getRegionArgument))
                                .then(CommandUtil.buildAddSubCommand(ArgumentUtil::getRegionArgument))
                                .then(CommandUtil.buildListSubCommand(ArgumentUtil::getRegionArgument))
                                .then(CommandUtil.buildRemoveSubCommand(ArgumentUtil::getRegionArgument))
                                .then(CommandUtil.buildCopySubCommand(ArgumentUtil::getRegionArgument))
                                .then(literal(ADD)
                                        .then(literal(CHILD)
                                                .then(Commands.argument(CHILD.toString(), StringArgumentType.word())
                                                        .suggests((ctx, builder) -> AddRegionChildArgumentType.potentialChildRegions().listSuggestions(ctx, builder))
                                                        .executes(ctx -> addChildren(ctx, getRegionArgument(ctx), getChildRegionArgument(ctx))))))
                                .then(literal(REMOVE)
                                        .then(literal(CHILD)
                                                .then(Commands.argument(CHILD.toString(), StringArgumentType.word())
                                                        .suggests((ctx, builder) -> RemoveRegionChildArgumentType.childRegions().listSuggestions(ctx, builder))
                                                        .executes(ctx -> removeChildren(ctx, getDimCacheArgument(ctx), getRegionArgument(ctx), getChildRegionArgument(ctx))))))
                                .then(literal(LIST)
                                        .then(literal(CHILDREN)
                                                .executes(ctx -> promptRegionChildren(ctx, getRegionArgument(ctx), 0))
                                                .then(Commands.argument(PAGE.toString(), IntegerArgumentType.integer(0))
                                                        .executes(ctx -> promptRegionChildren(ctx, getRegionArgument(ctx), getPageNoArgument(ctx))))))
                                .then(literal(STATE)
                                        .executes(ctx -> promptLocalRegionState(ctx, getRegionArgument(ctx)))
                                        .then(literal(ALERT)
                                                .executes(ctx -> CommandUtil.setAlertState(ctx, getRegionArgument(ctx), !getRegionArgument(ctx).isMuted()))
                                                .then(Commands.argument(ALERT.toString(), BoolArgumentType.bool())
                                                        .executes(ctx -> CommandUtil.setAlertState(ctx, getRegionArgument(ctx), getAlertArgument(ctx)))))
                                        .then(literal(ENABLE)
                                                .executes(ctx -> CommandUtil.setActiveState(ctx, getRegionArgument(ctx), !getRegionArgument(ctx).isActive()))
                                                .then(Commands.argument(ENABLE.toString(), BoolArgumentType.bool())
                                                        .executes(ctx -> CommandUtil.setActiveState(ctx, getRegionArgument(ctx), getEnableArgument(ctx)))))
                                        .then(literal(PRIORITY)
                                                .then(Commands.argument(PRIORITY.toString(), IntegerArgumentType.integer())
                                                        .executes(ctx -> setPriority(ctx, getRegionArgument(ctx), getPriorityArgument(ctx))))
                                                .then(literal(INC)
                                                        .then(Commands.argument(PRIORITY.toString(), IntegerArgumentType.integer())
                                                                .executes(ctx -> setPriority(ctx, getRegionArgument(ctx), getPriorityArgument(ctx), 1))))
                                                .then(literal(DEC)
                                                        .then(Commands.argument(PRIORITY.toString(), IntegerArgumentType.integer())
                                                                .executes(ctx -> setPriority(ctx, getRegionArgument(ctx), getPriorityArgument(ctx), -1))))))
                                .then(literal(AREA)
                                        .executes(ctx -> promptRegionAreaInfo(ctx.getSource(), getRegionArgument(ctx)))
                                        .then(literal(SET)
                                                .then(Commands.literal(AreaType.CUBOID.areaType)
                                                        .then(Commands.argument(POS1.toString(), BlockPosArgument.blockPos())
                                                                .then(Commands.argument(POS2.toString(), BlockPosArgument.blockPos())
                                                                        .executes(ctx -> updateArea(ctx, getRegionArgument(ctx), AreaType.CUBOID, BlockPosArgument.getSpawnablePos(ctx, POS1.toString()), BlockPosArgument.getSpawnablePos(ctx, POS2.toString())))))))
                                        .then(literal(EXPAND)
                                                .executes(ctx -> expandArea(ctx, getRegionArgument(ctx), MIN_BUILD_LIMIT, MAX_BUILD_LIMIT))
                                                .then(Commands.argument(Y_MIN.toString(), IntegerArgumentType.integer(MIN_BUILD_LIMIT, MAX_BUILD_LIMIT))
                                                        .then(Commands.argument(Y_MAX.toString(), IntegerArgumentType.integer(MIN_BUILD_LIMIT, MAX_BUILD_LIMIT))
                                                                .executes(ctx -> expandArea(ctx, getRegionArgument(ctx), IntegerArgumentType.getInteger(ctx, Y_MIN.toString()), IntegerArgumentType.getInteger(ctx, Y_MAX.toString()))))))
                                        .then(literal(TELEPORT)
                                                .then(Commands.literal(SET.toString())
                                                        .then(Commands.argument(TARGET.toString(), BlockPosArgument.blockPos())
                                                                .executes(ctx -> setTeleportPos(ctx, getRegionArgument(ctx), BlockPosArgument.getSpawnablePos(ctx, TARGET.toString())))))))
                                .then(literal(TELEPORT)
                                        .executes(ctx -> teleport(ctx.getSource(), getRegionArgument(ctx)))
                                        .then(Commands.argument(PLAYER.toString(), EntityArgument.player())
                                                .executes(ctx -> teleport(ctx.getSource(), getRegionArgument(ctx), getPlayerArgument(ctx)))))
                                .then(literal(RENAME)
                                        .then(Commands.argument(REGION.toString(), StringArgumentType.word())
                                                .executes(ctx -> renameRegion(ctx, getRegionArgument(ctx), getRegionNameArgument(ctx), getDimCacheArgument(ctx)))))
                        )
                );
    }

    // TODO: Needs rework after other shapes are implemented
    private static int expandArea(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, int yMin, int yMax) {
        int min = Math.min(yMin, yMax);
        int max = Math.max(yMin, yMax);
        IMarkableArea oldArea = region.getArea();
        if (oldArea instanceof CuboidArea) {
            CuboidArea cuboidArea = CuboidArea.expand((CuboidArea) oldArea, min, max);
            region.setArea(cuboidArea);
        } else {
            throw new IllegalArgumentException("Unexpected value = " + oldArea.getClass().getName());
        }
        RegionDataManager.save();
        MutableComponent updateAreaMsg = new TranslatableComponent("cli.msg.info.region.spatial.area.update", buildRegionSpatialPropLink(region), buildRegionInfoLink(region));
        sendCmdFeedback(ctx.getSource(), updateAreaMsg);
        return 0;
    }

    private static int copyPlayers(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, IMarkableRegion srcRegion, String affiliation) {

        RegionDataManager.save();
        return 0;
    }

    private static int copyPlayers(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, IMarkableRegion srcRegion) {
        return copyPlayers(ctx, region, srcRegion, "member") + copyPlayers(ctx, region, srcRegion, "owners");
    }

    private static int copyTeams(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, IMarkableRegion srcRegion, String affiliation) {
        RegionDataManager.save();
        return 0;
    }

    private static int copyTeams(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, IMarkableRegion srcRegion) {
        return copyTeams(ctx, region, srcRegion, "member") + copyTeams(ctx, region, srcRegion, "owners");
    }

    private static int copyAffiliation(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, IMarkableRegion srcRegion, String affiliation) {
        RegionDataManager.save();
        return 0;
    }



    private static int updateArea(CommandContext<CommandSourceStack> src, IMarkableRegion region, AreaType areaType, BlockPos pos1, BlockPos pos2) {
        try {
            IProtectedRegion parent = region.getParent();
            // TODO: Contains method for regions, with dimensional always returning true if dim is the same
            // IMarkableRegions would use the area contains method
            switch (areaType) {
                case CUBOID:
                    CuboidArea cuboidArea = new CuboidArea(pos1, pos2);
                    CuboidRegion cuboidRegion = (CuboidRegion) region;
                    if (parent instanceof DimensionalRegion) {
                        int newPriority = LocalRegions.ensureHigherRegionPriorityFor(cuboidRegion, RegionConfig.getDefaultPriority());
                        YetAnotherWorldProtector.LOGGER.info("New priority {} for region {}", newPriority, region.getName());
                    }
                    if (parent instanceof IMarkableRegion localParentRegion) {
                        CuboidArea parentArea = (CuboidArea) localParentRegion.getArea();
                        if (parentArea.contains(cuboidArea)) {
                            int newPriority = LocalRegions.ensureHigherRegionPriorityFor(cuboidRegion, localParentRegion.getPriority() + 1);
                            YetAnotherWorldProtector.LOGGER.info("New priority {} for region {}", newPriority, region.getName());
                        } else {
                            MutableComponent updateAreaFailMsg = new TranslatableComponent("cli.msg.info.region.spatial.area.update.fail.boundaries", buildRegionInfoLink(parent), buildRegionInfoLink(region));
                            sendCmdFeedback(src.getSource(), updateAreaFailMsg);
                            return 1;
                        }
                    }
                    cuboidRegion.setArea(cuboidArea);
                    RegionDataManager.save();
                    MutableComponent updateAreaMsg = new TranslatableComponent("cli.msg.info.region.spatial.area.update", buildRegionSpatialPropLink(region), buildRegionInfoLink(region));
                    sendCmdFeedback(src.getSource(), updateAreaMsg);
                    return 0;
                case CYLINDER:
                case SPHERE:
                case POLYGON_3D:
                case PRISM:
                    throw new UnsupportedOperationException("Unsupported region type");
            }
            return 0;
        } catch (Exception ex) {
            YetAnotherWorldProtector.LOGGER.error("Failed to update area: {}", ex.getMessage());
            return 1;
        }
    }

    private static int renameRegion(CommandContext<CommandSourceStack> src, IMarkableRegion region, String regionName, DimensionRegionCache dimCache) {
        int res = checkValidRegionName(regionName, dimCache);
        if (res == -1) {
            sendCmdFeedback(src.getSource(), new TranslatableComponent("cli.msg.dim.info.region.create.name.invalid", regionName));
            return res;
        }
        if (res == 1) {
            sendCmdFeedback(src.getSource(), new TranslatableComponent("cli.msg.dim.info.region.create.name.exists", dimCache.getDimensionalRegion().getName(), buildRegionInfoLink(dimCache.getRegion(regionName))));
            return res;
        }
        // FIXME:
        dimCache.renameRegion(region, regionName);
        RegionDataManager.save();
        return 0;
    }

    private static int removeTeam(CommandContext<CommandSourceStack> src, PlayerTeam team, IMarkableRegion region, String affiliation) {
        MutableComponent undoLink = buildRegionActionUndoLink(src.getInput(), REMOVE, ADD);
        switch (affiliation) {
            case "member":
                if (region.hasMember(team.getName())) {
                    region.removeMember(team);
                    RegionDataManager.save();
                    sendCmdFeedback(src.getSource(), new TranslatableComponent("cli.msg.info.region.affiliation.team.removed",
                            team.getName(), buildRegionInfoLink(region)).append(" ").append(undoLink));
                }
                break;
            case "owner":
                if (region.hasOwner(team.getName())) {
                    region.removeOwner(team);
                    RegionDataManager.save();
                    sendCmdFeedback(src.getSource(), new TranslatableComponent("cli.msg.info.region.affiliation.team.removed",
                            team.getName(), buildRegionInfoLink(region)).append(" ").append(undoLink));
                }
                break;
            default:
                return 1;
        }
        return 0;
    }

    private static int addTeam(CommandContext<CommandSourceStack> src, PlayerTeam team, IMarkableRegion region, String affiliation) {
        MutableComponent undoLink = buildRegionActionUndoLink(src.getInput(), ADD, REMOVE);
        switch (affiliation) {
            case "member":
                if (!region.hasMember(team.getName())) {
                    region.addMember(team);
                    RegionDataManager.save();
                    sendCmdFeedback(src.getSource(), new TranslatableComponent("cli.msg.info.region.affiliation.team.added",
                            team.getName(), affiliation, buildRegionInfoLink(region)).append(" ").append(undoLink));
                }
                break;
            case "owner":
                if (!region.hasOwner(team.getName())) {
                    region.addOwner(team);
                    RegionDataManager.save();
                    sendCmdFeedback(src.getSource(), new TranslatableComponent("cli.msg.info.region.affiliation.team.added",
                            team.getName(), affiliation, buildRegionInfoLink(region)).append(" ").append(undoLink));
                }
                break;
            default:
                return 1;
        }
        return 0;
    }

    // TODO: Option to remove player by name
    private static int removePlayer(CommandContext<CommandSourceStack> src, String playerName, IMarkableRegion region, String affiliation) {
        return 1;
    }

    private static int removePlayer(CommandContext<CommandSourceStack> src, ServerPlayer player, IMarkableRegion region, String affiliation) {
        MutableComponent undoLink = buildRegionActionUndoLink(src.getInput(), REMOVE, ADD);
        switch (affiliation) {
            case "member":
                if (region.hasMember(player.getUUID())) {
                    region.removeMember(player);
                    RegionDataManager.save();
                    sendCmdFeedback(src.getSource(), new TranslatableComponent("cli.msg.info.region.affiliation.player.removed",
                            buildPlayerHoverComponent(player), buildRegionInfoLink(region)).append(" ").append(undoLink));
                }
                break;
            case "owner":
                if (region.hasOwner(player.getUUID())) {
                    region.removeOwner(player);
                    RegionDataManager.save();
                    sendCmdFeedback(src.getSource(), new TranslatableComponent("cli.msg.info.region.affiliation.player.removed",
                            buildPlayerHoverComponent(player), buildRegionInfoLink(region)).append(" ").append(undoLink));
                }
                break;
            default:
                return 1;
        }
        return 0;
    }

    private static int addPlayer(CommandContext<CommandSourceStack> src, ServerPlayer player, IMarkableRegion region, String affiliation) {
        MutableComponent undoLink = buildRegionActionUndoLink(src.getInput(), ADD, REMOVE);
        switch (affiliation) {
            case "member":
                if (!region.hasMember(player.getUUID())) {
                    region.addMember(player);
                    RegionDataManager.save();
                    sendCmdFeedback(src.getSource(), new TranslatableComponent("cli.msg.info.region.affiliation.player.added",
                            buildPlayerHoverComponent(player), affiliation, buildRegionInfoLink(region)).append(" ").append(undoLink));
                }
                break;
            case "owner":
                if (!region.hasOwner(player.getUUID())) {
                    region.addOwner(player);
                    RegionDataManager.save();
                    sendCmdFeedback(src.getSource(), new TranslatableComponent("cli.msg.info.region.affiliation.player.added",
                            buildPlayerHoverComponent(player), affiliation, buildRegionInfoLink(region)).append(" ").append(undoLink));
                }
                break;
            default:
                return 1;
        }
        return 0;
    }

    private static int removeChildren(CommandContext<CommandSourceStack> src, DimensionRegionCache dimCache, IMarkableRegion parent, IMarkableRegion child) {
        if (parent.hasChild(child)) {
            // FIXME: Removing child does not set priority correct with overlapping regions
            dimCache.getDimensionalRegion().addChild(child); // this also removes the child from the local parent
            child.setIsActive(false);
            LocalRegions.ensureLowerRegionPriorityFor((CuboidRegion) child, RegionConfig.getDefaultPriority());
            RegionDataManager.save();
            MutableComponent parentLink = buildRegionInfoLink(parent);
            MutableComponent notLongerChildLink = buildRegionInfoLink(child);
            MutableComponent dimensionalLink = buildRegionInfoLink(dimCache.getDimensionalRegion(), RegionType.DIMENSION);
            sendCmdFeedback(src.getSource(), new TranslatableComponent("cli.msg.info.region.children.remove", notLongerChildLink, parentLink));
            sendCmdFeedback(src.getSource(), new TranslatableComponent("cli.msg.info.region.parent.clear", notLongerChildLink, dimensionalLink));
            return 0;
        }
        // should not happen, due to RemoveRegionChildArgumentType should only provide valid child regions
        return -1;
    }

    private static int addChildren(CommandContext<CommandSourceStack> src, IMarkableRegion parent, IMarkableRegion child) {
        if (!parent.hasChild(child) && child.getParent() != null && child.getParent() instanceof DimensionalRegion) {
            parent.addChild(child);
            LocalRegions.ensureHigherRegionPriorityFor((CuboidRegion) child, parent.getPriority() + 1);
            RegionDataManager.save();
            MutableComponent parentLink = buildRegionInfoLink(parent);
            MutableComponent childLink = buildRegionInfoLink(child);
            MutableComponent undoLink = buildRegionActionUndoLink(src.getInput(), ADD, REMOVE);
            sendCmdFeedback(src.getSource(), new TranslatableComponent("cli.msg.info.region.children.add", childLink, parentLink).append(" ").append(undoLink));
            return 0;
        }
        // should not happen, due to AddRegionChildArgumentType should only provide valid child regions
        return -1;
    }


    // Adds default flag for provided RegionFlag
    private static int addFlag(CommandContext<CommandSourceStack> src, IMarkableRegion region, RegionFlag flag) {
        if (!region.containsFlag(flag)) {
            IFlag iFlag = null;
            switch (flag.type) {
                case BOOLEAN_FLAG:
                    iFlag = new BooleanFlag(flag);
                    region.addFlag(iFlag);
                    break;
                case LIST_FLAG:
                case INT_FLAG:
                    break;
            }
            // TODO: More general: Trigger for adding flags?
            if (flag.name.contains("spawning") && FlagConfig.removeEntitiesEnabled()) {
                removeInvolvedEntities(src, region, flag);
            }
            RegionDataManager.save();
            // TODO: flag cmd info link
            sendCmdFeedback(src.getSource(), new TranslatableComponent("cli.msg.flags.added", buildFlagInfoLink(region, iFlag),
                    buildRegionInfoLink(region)).append(" ").append(buildRegionActionUndoLink(src.getInput(), ADD, REMOVE)));
            return 0;
        }
        return 1;
    }

    public static void removeInvolvedEntities(CommandContext<CommandSourceStack> src, IProtectedRegion region, RegionFlag flag) {
        // FIXME: Level is where the command source is, not the target level of the region
        ServerLevel level = src.getSource().getLevel();
        Predicate<? super Entity> entityFilter = getEntityFilterForFlag(flag);
        if (region instanceof DimensionalRegion) {
            List<Entity> entities = getEntitiesToRemove(level, entityFilter, flag);
            entities.forEach(e -> e.setRemoved(Entity.RemovalReason.DISCARDED));
        }
        if (region instanceof IMarkableRegion) {
            List<Entity> entities = getEntitiesToRemove(level, (IMarkableRegion) region, entityFilter);
            entities.forEach(e -> e.setRemoved(Entity.RemovalReason.DISCARDED));
        }
        if (region instanceof GlobalRegion) {
            Map<ServerLevel, List<Entity>> entities = getEntitiesToRemove((GlobalRegion) region, entityFilter);
            entities.forEach((world, entityList) -> {
                entityList.forEach(e -> e.setRemoved(Entity.RemovalReason.DISCARDED));
            });
        }
    }

    private static Predicate<? super Entity> getEntityFilterForFlag(RegionFlag flag) {
        switch (flag) {
            case SPAWNING_ALL:
                // FIXME: does not remove ExperienceOrbEntity
                return e -> !(e instanceof Player);
            case SPAWNING_MONSTER:
                return HandlerUtil::isMonster;
            case SPAWNING_ANIMAL:
                return HandlerUtil::isAnimal;
            case SPAWNING_GOLEM:
                return e -> e instanceof AbstractGolem;
            case SPAWNING_TRADER:
                return e -> e instanceof WanderingTrader;
            case SPAWNING_SLIME:
                return e -> e instanceof Slime;
            case SPAWNING_VILLAGER:
                return HandlerUtil::isVillager;
            case SPAWNING_XP:
                return e -> e instanceof ExperienceOrb;
            default:
                return e -> false;
        }
    }

    private static List<Entity> getEntitiesToRemove(ServerLevel level, IMarkableRegion region, Predicate<? super Entity> entityFilter) {
        // TODO: make this work with areas of different shapes by manually implementing it
        // TODO: Use the predicate to determine if the entity is within the region
        // area.intersects(entity.getBoundingBox()) ...
        return level.getEntities((Entity) null, ((CuboidArea) region.getArea()).getArea(), entityFilter);
    }

    private static Map<ServerLevel, List<Entity>> getEntitiesToRemove(GlobalRegion region, Predicate<? super Entity> entityFilter) {
        // FIXME: Exclude entities from Local Regions or DimensionalRegions with flag
        return new HashMap<>();
    }

    private static List<Entity> getEntitiesToRemove(ServerLevel level, Predicate<? super Entity> entityFilter, RegionFlag flag) {
        List<? extends Entity> entities = level.getEntities(EntityTypeTest.forClass(Entity.class), entityFilter);
        // List<Entity> entities = level.getEntities(null, entityFilter);
        // don't consider entities, which are currently in a Local Region which doesn't have the flag
        // TODO: fixme after flags can be negated (either flag is not existent or deactivated...)
        return entities.stream()
                .filter(e -> isInRegionWithoutFlag(level, flag, e))
                .collect(Collectors.toList());
    }

    private static boolean isInRegionWithoutFlag(ServerLevel level, RegionFlag flag, Entity e) {
        return LocalRegions.getRegionWithoutFlag(flag, e.blockPosition(), level.dimension()) == null;
    }

    private static int removeFlag(CommandContext<CommandSourceStack> src, IMarkableRegion region, RegionFlag flag) {
        if (region.containsFlag(flag)) {
            region.removeFlag(flag.name);
            RegionDataManager.save();
            sendCmdFeedback(src.getSource(), new TranslatableComponent("cli.msg.flags.removed",
                    flag.name, buildRegionInfoLink(region)).append(" ").append(buildRegionActionUndoLink(src.getInput(), REMOVE, ADD)));
            return 0;
        }
        return 1;
    }

    private static int setAlertState(CommandContext<CommandSourceStack> src, IMarkableRegion region, boolean showAlert) {
        boolean oldState = !region.isMuted();
        region.setIsMuted(showAlert);
        RegionDataManager.save();
        if (oldState == region.isMuted()) {
            boolean isEnabled = !region.isMuted();
            MutableComponent undoLink = buildRegionActionUndoLink(src.getInput(), showAlert ? TRUE : FALSE, showAlert ? FALSE : TRUE);
            sendCmdFeedback(src.getSource(), new TranslatableComponent("cli.msg.info.region.state.alert.set.value",
                    buildRegionInfoLink(region), oldState, isEnabled).append(" ").append(undoLink));
        }
        return 0;
    }

    private static int setAlertState(CommandContext<CommandSourceStack> src, IMarkableRegion region) {
        return setAlertState(src, region, !region.isMuted());
    }

    private static int setEnableState(CommandContext<CommandSourceStack> src, IMarkableRegion region, boolean enable) {
        boolean oldState = region.isActive();
        region.setIsActive(enable);
        RegionDataManager.save();
        if (oldState != region.isActive()) {
            MutableComponent undoLink = buildRegionActionUndoLink(src.getInput(), enable ? TRUE : FALSE, enable ? FALSE : TRUE);
            sendCmdFeedback(src.getSource(), new TranslatableComponent("cli.msg.info.region.state.enable.set.value",
                    buildRegionInfoLink(region), oldState, region.isActive()).append(" ").append(undoLink));
        }
        return 0;
    }

    private static int setEnableState(CommandContext<CommandSourceStack> src, IMarkableRegion region) {
        return setEnableState(src, region, !region.isActive());
    }

    private static int setPriority(CommandContext<CommandSourceStack> src, IMarkableRegion region, int priority, int factor) {
        long newValue = (long) region.getPriority() + ((long) priority * factor);
        if (Integer.MAX_VALUE - newValue > 0) {
            return setPriority(src, region, (int) newValue);
        } else {
            sendCmdFeedback(src.getSource(), new TranslatableComponent("cli.msg.warn.region.state.priority.set.invalid", buildRegionInfoLink(region), newValue));
            return -1;
        }
    }

    /**
     * Attempt to set new priority for the given region. <br>
     * Fails if region priority is used by an overlapping region at same hierarchy level.
     *
     * @param src
     * @param region
     * @param priority
     * @return
     */
    private static int setPriority(CommandContext<CommandSourceStack> src, IMarkableRegion region, int priority) {
        CuboidRegion cuboidRegion = (CuboidRegion) region;
        List<CuboidRegion> intersectingRegions = LocalRegions.getIntersectingRegionsFor(cuboidRegion);
        boolean existRegionWithSamePriority = intersectingRegions
                .stream()
                .anyMatch(r -> r.getPriority() == priority);
        IProtectedRegion parent = region.getParent();
        if (parent instanceof IMarkableRegion) {
            int parentPriority = ((IMarkableRegion) parent).getPriority();
            if (parentPriority >= priority) {
                MutableComponent updatePriorityFailMsg = new TranslatableComponent("cli.msg.info.region.state.priority.set.fail.to-low", buildRegionInfoLink(region));
                sendCmdFeedback(src.getSource(), updatePriorityFailMsg);
                return 1;
            }
        }
        if (existRegionWithSamePriority) {
            MutableComponent updatePriorityFailMsg = new TranslatableComponent("cli.msg.info.region.state.priority.set.fail.same", buildRegionInfoLink(region), priority);
            sendCmdFeedback(src.getSource(), updatePriorityFailMsg);
            return 1;
        } else {
            int oldPriority = region.getPriority();
            if (oldPriority != priority) {
                region.setPriority(priority);
                RegionDataManager.save();
                MutableComponent undoLink = buildRegionActionUndoLink(src.getInput(), String.valueOf(oldPriority), String.valueOf(priority));
                sendCmdFeedback(src.getSource(), new TranslatableComponent("cli.msg.info.region.state.priority.set.success",
                        buildRegionInfoLink(region), oldPriority, region.getPriority()).append(" ").append(undoLink));
                return 0;
            } else {
                sendCmdFeedback(src.getSource(), new TranslatableComponent("cli.msg.info.region.state.priority.set.fail.no-change", buildRegionInfoLink(region)));
                return 1;
            }
        }
    }

    private static int promptRegionInfo(CommandSourceStack src, IMarkableRegion region) {
        // == Region [<name>] overview ==
        sendCmdFeedback(src, buildRegionOverviewHeader(region));
        // Flags: [n flag(s)][+]
        sendCmdFeedback(src, buildInfoComponent("cli.msg.info.region.flag", buildFlagListLink(region, RegionType.LOCAL)));
        // Spatial: [Spatial Properties]
        sendCmdFeedback(src, buildInfoComponent("cli.msg.info.region.spatial", buildRegionSpatialPropLink(region)));
        // Affiliations: [owners], [members], [<listAffiliations>]
        sendCmdFeedback(src, buildInfoComponent("cli.msg.info.region.affiliation", buildAffiliationLinks(region, RegionType.LOCAL)));
        // Hierarchy: [parent][-|+], [n children][+]
        MutableComponent regionHierarchy = new TranslatableComponent("cli.msg.info.region.hierarchy")
                .append(": ")
                .append(buildRegionParentLink(region))
                .append(new TextComponent(", ").withStyle(ChatFormatting.RESET))
                .append(buildRegionChildrenLink(region));
        sendCmdFeedback(src, regionHierarchy);
        // State: [State]
        sendCmdFeedback(src, buildInfoComponent("cli.msg.info.region.state", buildRegionStateLink(region)));
        return 0;
    }

    private static int promptRegionChildren(CommandSourceStack src, IMarkableRegion region, int pageNo) {
        List<IMarkableRegion> children = region.getChildren().values().stream().map(r -> (IMarkableRegion) r).collect(Collectors.toList());
        MutableComponent childRegionList = new TextComponent("");
        if (children.isEmpty()) {
            MutableComponent noChildrenText = new TranslatableComponent("cli.msg.info.region.children.empty", buildRegionInfoLink(region));
            childRegionList.append(noChildrenText);
            sendCmdFeedback(src, childRegionList);
        }
        List<MutableComponent> regionPagination = buildPaginationComponents(
                buildRegionChildrenHeader(region),
                buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), LIST.toString(), CHILDREN.toString()),
                buildRemoveRegionEntries(region, children),
                pageNo,
                new TextComponent(" - ").append(buildRegionAddChildrenLink(region)));
        regionPagination.forEach(line -> sendCmdFeedback(src, line));
        return 0;
    }

    /**
     * == Affiliation '%s' for '%s'==
     * Players: [n player(s)][+]
     * Teams: [m team(s)][+]
     */
    private static int promptRegionAffiliates(CommandSourceStack src, IMarkableRegion region, String affiliation) {
        sendCmdFeedback(src, buildAffiliationHeader(region, affiliation, RegionType.LOCAL));
        sendCmdFeedback(src, buildAffiliationPlayerListLink(region, affiliation, RegionType.LOCAL));
        sendCmdFeedback(src, buildAffiliationTeamListLink(region, affiliation, RegionType.LOCAL));
        return 0;
    }

    private static int promptRegionAffiliationList(CommandSourceStack src, IMarkableRegion region, String affiliation, GroupType affiliationType, int pageNo) {
        List<String> affiliateNames = getAffiliateList(region, affiliation, affiliationType);
        if (affiliateNames.isEmpty()) {
            sendCmdFeedback(src, new TranslatableComponent("cli.msg.info.region.affiliation." + affiliationType.name + ".empty", affiliation, buildRegionInfoLink(region)));
            return 1;
        }
        List<MutableComponent> regionPagination = buildPaginationComponents(
                buildAffiliationHeader(region, affiliation, affiliationType, RegionType.LOCAL),
                buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), LIST.toString(), affiliation, affiliationType.name),
                buildRemoveAffiliationEntries(region, affiliateNames, affiliationType, affiliation, RegionType.LOCAL),
                pageNo,
                new TextComponent(" - ").append(buildAddAffiliateLink(region, affiliation, affiliationType, RegionType.LOCAL)));
        regionPagination.forEach(line -> sendCmdFeedback(src, line));
        return 0;
    }

    /**
     * Prompt region spatial properties like teleport location and area.
     * == Region [<name>] spatial properties ==
     * Location: [dimInfo]@[tpCoordinates]
     * Area: [spatialProperties]
     * @param src
     * @param region
     * @return
     */
    public static int promptRegionSpatialProperties(CommandSourceStack src, IMarkableRegion region) {
        sendCmdFeedback(src, buildHeader(new TranslatableComponent("cli.msg.info.header.for", buildRegionSpatialPropLink(region), buildRegionInfoLink(region))));
        sendCmdFeedback(src, buildInfoComponent("cli.msg.info.region.spatial.location", buildDimensionTeleportLink(region)));
        sendCmdFeedback(src, buildInfoComponent("cli.msg.info.region.spatial.area", buildRegionAreaDetailComponent(region)));
        return 0;
    }

    /**
     * Prompt the region state to the command issuer.
     * ==  [state] for [<name>]  ==
     * Enabled: [true|false]
     * Priority: n [#][+5][-5]
     * Alert: [on|off]
     *
     * @param src
     * @param region
     * @return
     */
    public static int promptRegionState(CommandSourceStack src, IMarkableRegion region) {
        sendCmdFeedback(src, buildHeader(new TranslatableComponent("cli.msg.info.header.for", buildRegionStateLink(region), buildRegionInfoLink(region))));
        sendCmdFeedback(src, buildInfoComponent("cli.msg.info.region.state.priority", buildRegionPriorityComponent(region)));
        sendCmdFeedback(src, buildInfoComponent("cli.msg.info.region.state.enable", buildRegionEnableComponent(region)));
        sendCmdFeedback(src, buildInfoComponent("cli.msg.info.region.state.alert", buildRegionAlertComponentLink(region)));
        return 0;
    }

    public static int promptRegionFlags(CommandSourceStack src, IMarkableRegion region, int pageNo) {
        if (region.getFlags().isEmpty()) {
            sendCmdFeedback(src, new TranslatableComponent("cli.msg.info.region.flag.empty", buildRegionInfoLink(region)));
            return 1;
        }
        List<IFlag> flags = LocalRegions.getSortedFlags(region);
        List<MutableComponent> flagPagination = buildPaginationComponents(
                buildRegionFlagInfoHeader(region),
                buildCommandStr(REGION.toString(), region.getDim().location().toString(), region.getName(), LIST.toString(), FLAG.toString()),
                buildRemoveFlagEntries(region, flags),
                pageNo,
                new TextComponent(" - ").append(buildRegionAddFlagLink(region))
        );
        flagPagination.forEach(line -> sendCmdFeedback(src, line));
        return 0;
    }

    // TODO: Only with region marker
    // assumption: regions are only updated with the region marker when in the same dimension
    private static int updateRegion(CommandContext<CommandSourceStack> src, IMarkableRegion region) {
        try {
            Player player = src.getSource().getPlayerOrException();
            ItemStack maybeStick = player.getMainHandItem();
            if (StickUtil.isVanillaStick(maybeStick)) {
                try {
                    AbstractStick abstractStick = StickUtil.getStick(maybeStick);
                    if (abstractStick.getStickType() == StickType.MARKER) {
                        MarkerStick marker = (MarkerStick) abstractStick;
                        // TODO: RegionDataManager.get().update(regionName, marker);
                    }
                } catch (StickException e) {
                    sendCmdFeedback(src.getSource(), "CommandSourceStack is not player. Aborting.. Needs RegionMarker with Block-NBT data in player hand");
                }
            }
        } catch (CommandSyntaxException e) {
            e.printStackTrace();
        }
        return 0;
    }

    private static int teleport(CommandSourceStack src, IMarkableRegion region) {
        try {
            ServerPlayer player = src.getPlayerOrException();
            src.getServer().getCommands().getDispatcher().execute(buildRegionTpCmd(region, player.getScoreboardName()), src);
            return 0;
        } catch (CommandSyntaxException e) {
            YetAnotherWorldProtector.LOGGER.warn("Unable to teleport command source to region.");
            return -1;
        }
    }

    private static int teleport(CommandSourceStack src, IMarkableRegion region, Player player) {
        try {
            src.getServer().getCommands().getDispatcher().execute(buildRegionTpCmd(region, player.getScoreboardName()), src);
            return 0;
        } catch (CommandSyntaxException e) {
            YetAnotherWorldProtector.LOGGER.warn("Error executing teleport command.");
            // TODO: error executing tp command
            return -1;
        }
    }

    // Todo: Enable/Disable teleporting? - Only for owners?
    private static int setTeleportPos(CommandContext<CommandSourceStack> src, IMarkableRegion region, BlockPos target) {
        if (!region.getTpTarget().equals(target)) {
            region.setTpTarget(target);
            RegionDataManager.save();
            MutableComponent newTpTargetLink = buildDimensionalBlockTpLink(region.getDim(), target);
            sendCmdFeedback(src.getSource(), new TranslatableComponent("cli.msg.info.region.spatial.location.teleport.set", buildRegionInfoLink(region), newTpTargetLink));
            return 0;
        }
        return 1;
    }

}
