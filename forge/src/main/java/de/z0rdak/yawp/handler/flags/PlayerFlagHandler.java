package de.z0rdak.yawp.handler.flags;

import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.api.events.region.FlagCheckResult;
import de.z0rdak.yawp.constants.Constants;
import de.z0rdak.yawp.core.flag.FlagState;
import de.z0rdak.yawp.data.region.DimensionRegionCache;
import de.z0rdak.yawp.data.region.RegionDataManager;
import de.z0rdak.yawp.handler.HandlerUtil;
import de.z0rdak.yawp.platform.Services;
import de.z0rdak.yawp.util.text.MessageSender;
import net.minecraft.core.BlockPos;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.Container;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.MenuProvider;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.monster.Monster;
import net.minecraft.world.entity.npc.Villager;
import net.minecraft.world.entity.npc.WanderingTrader;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;
import net.minecraft.world.item.context.UseOnContext;
import net.minecraft.world.level.Explosion;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.BasePressurePlateBlock;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.SimpleWaterloggedBlock;
import net.minecraft.world.level.block.entity.BaseContainerBlockEntity;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.entity.EnderChestBlockEntity;
import net.minecraft.world.level.block.entity.LecternBlockEntity;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.properties.BlockStateProperties;
import net.minecraft.world.level.material.Fluid;
import net.minecraft.world.phys.AABB;
import net.minecraft.world.phys.BlockHitResult;
import net.minecraft.world.phys.HitResult;
import net.minecraftforge.common.ToolActions;
import net.minecraftforge.event.CommandEvent;
import net.minecraftforge.event.ServerChatEvent;
import net.minecraftforge.event.TickEvent;
import net.minecraftforge.event.entity.EntityMountEvent;
import net.minecraftforge.event.entity.EntityTeleportEvent;
import net.minecraftforge.event.entity.item.ItemTossEvent;
import net.minecraftforge.event.entity.living.*;
import net.minecraftforge.event.entity.player.*;
import net.minecraftforge.event.level.BlockEvent;
import net.minecraftforge.event.level.ExplosionEvent;
import net.minecraftforge.eventbus.api.Event;
import net.minecraftforge.eventbus.api.EventPriority;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.registries.ForgeRegistries;
import net.minecraftforge.registries.tags.ITagManager;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Consumer;

import static de.z0rdak.yawp.core.flag.RegionFlag.*;
import static de.z0rdak.yawp.handler.HandlerUtil.*;
import static de.z0rdak.yawp.util.text.MessageSender.sendFlagMsg;
import static net.minecraftforge.fml.common.Mod.EventBusSubscriber.Bus.FORGE;

/**
 * Contains flag handler for events directly related/cause to/by players.
 */
@Mod.EventBusSubscriber(modid = Constants.MOD_ID, bus = FORGE)
public final class PlayerFlagHandler {

    private PlayerFlagHandler() {
    }

    @SubscribeEvent
    public static void onElytraFlying(TickEvent.PlayerTickEvent event) {
        if (isServerSide(event.player) && event.phase == TickEvent.Phase.END) {
            ResourceKey<Level> dim = getDimKey(event.player);
            if (event.player.isFallFlying()) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(event.player.blockPosition(), NO_FLIGHT, dim, event.player);
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
                processCheck(checkEvent, onDeny -> {
                    event.player.stopFallFlying();
                });
            }
        }
    }

    /**
     * Prevents traditional attacks from players which use EntityPlayer.attackTargetEntityWithCurrentItem(Entity).
     */
    @SubscribeEvent
    public static void onAttackPlayer(AttackEntityEvent event) {
        if (ForgeHandlerUtil.notServerSideOrPlayerNull(event)) return;
        if (event.getTarget() instanceof Player target) {
            Player attacker = event.getEntity();
            ResourceKey<Level> dim = getDimKey(attacker);
            FlagCheckEvent checkEvent = new FlagCheckEvent(target.blockPosition(), MELEE_PLAYERS, dim, attacker);
            if (Services.EVENT.post(checkEvent)) {
                return;
            }
            processCheck(checkEvent, onDeny -> {
                event.setCanceled(true);
                sendFlagMsg(onDeny);
            });
        }

    }

    /**
     * Prevents various entities from been attacked from a player. <br>
     * TODO: Flag for all entities
     */
    @SubscribeEvent
    public static void onAttackEntity(AttackEntityEvent event) {
        if (ForgeHandlerUtil.notServerSideOrPlayerNull(event)) return;
        Player player = event.getEntity();
        Entity eventEntity = event.getTarget();
        ResourceKey<Level> dim = getDimKey(event.getEntity());
        BlockPos entityPos = eventEntity.blockPosition();
        FlagCheckEvent checkEvent = null;

        if (isAnimal(eventEntity)) {
            checkEvent = new FlagCheckEvent(entityPos, MELEE_ANIMALS, dim, player);
            if (Services.EVENT.post(checkEvent)) {
                return;
            }
        }
        if (isMonster(eventEntity)) {
            checkEvent = new FlagCheckEvent(entityPos, MELEE_MONSTERS, dim, player);
            if (Services.EVENT.post(checkEvent)) {
                return;
            }
        }
        if (event.getTarget() instanceof Villager) {
            checkEvent = new FlagCheckEvent(entityPos, MELEE_VILLAGERS, dim, player);
            if (Services.EVENT.post(checkEvent)) {
                return;
            }
        }
        if (event.getTarget() instanceof WanderingTrader) {
            checkEvent = new FlagCheckEvent(entityPos, MELEE_WANDERING_TRADER, dim, player);
            if (Services.EVENT.post(checkEvent)) {
                return;
            }
        }
        if (checkEvent != null) {
            processCheck(checkEvent, onDeny -> {
                event.setCanceled(true);
                sendFlagMsg(onDeny);
            });
        }

    }

    @SubscribeEvent
    public static void onPickupItem(EntityItemPickupEvent event) {
        if (ForgeHandlerUtil.notServerSideOrPlayerNull(event)) return;
        FlagCheckEvent checkEvent = new FlagCheckEvent(event.getEntity().blockPosition(), ITEM_PICKUP, getDimKey(event.getEntity()), event.getEntity());
        if (Services.EVENT.post(checkEvent)) {
            return;
        }
        processCheck(checkEvent, onDeny -> {
            event.setCanceled(true);
            sendFlagMsg(onDeny);
        });

    }

    @SubscribeEvent
    public static void onBreedingAttempt(BabyEntitySpawnEvent event) {
        Player player = event.getCausedByPlayer();
        if (player == null) {
            return;
        }
        if (!player.getCommandSenderWorld().isClientSide) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(event.getParentB().blockPosition(), ANIMAL_BREEDING, getDimKey(player), event.getCausedByPlayer());
            if (Services.EVENT.post(checkEvent)) {
                return;
            }
            processCheck(checkEvent, onDeny -> {
                event.setCanceled(true);
                sendFlagMsg(onDeny);
            });
        }
    }

    @SubscribeEvent
    public static void onAnimalTameAttempt(AnimalTameEvent event) {
        Player player = event.getTamer();
        if (player == null) {
            return;
        }
        if (!player.getCommandSenderWorld().isClientSide) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(event.getAnimal().blockPosition(), ANIMAL_TAMING, getDimKey(player), player);
            if (Services.EVENT.post(checkEvent)) {
                return;
            }
            processCheck(checkEvent, onDeny -> {
                event.setCanceled(true);
                sendFlagMsg(onDeny);
            });
        }
    }

    @SubscribeEvent
    public static void onPlayerLevelChange(PlayerXpEvent.LevelChange event) {
        if (ForgeHandlerUtil.notServerSideOrPlayerNull(event)) return;
        Player player = event.getEntity();
        FlagCheckEvent checkEvent = new FlagCheckEvent(event.getEntity().blockPosition(), LEVEL_FREEZE, getDimKey(player), player);
        if (Services.EVENT.post(checkEvent)) {
            return;
        }
        processCheck(checkEvent, onDeny -> {
            event.setCanceled(true);
            sendFlagMsg(onDeny);
        });

    }

    @SubscribeEvent
    public static void onPlayerXPChange(PlayerXpEvent.XpChange event) {
        if (ForgeHandlerUtil.notServerSideOrPlayerNull(event)) return;
        Player player = event.getEntity();
        FlagCheckEvent checkEvent = new FlagCheckEvent(event.getEntity().blockPosition(), XP_FREEZE, getDimKey(player), player);
        if (Services.EVENT.post(checkEvent)) {
            return;
        }
        processCheck(checkEvent, onDeny -> {
            event.setCanceled(true);
            event.setAmount(0);
            sendFlagMsg(onDeny);
        });

    }

    @SubscribeEvent
    public static void onPlayerXpPickup(PlayerXpEvent.PickupXp event) {
        if (ForgeHandlerUtil.notServerSideOrPlayerNull(event)) return;
        Player player = event.getEntity();
        FlagCheckEvent checkEvent = new FlagCheckEvent(event.getEntity().blockPosition(), XP_PICKUP, getDimKey(player), player);
        if (Services.EVENT.post(checkEvent)) {
            return;
        }
        processCheck(checkEvent, onDeny -> {
            event.setCanceled(true);
            event.getOrb().remove(Entity.RemovalReason.DISCARDED);
            sendFlagMsg(onDeny);
        });

    }

    @SubscribeEvent
    public static void onPvpAction(LivingHurtEvent event) {
        if (ForgeHandlerUtil.isServerSide(event)) {
            if (event.getSource() == null || event.getEntity() == null) return;
            Entity dmgSourceEntity = event.getSource().getDirectEntity();
            Entity hurtEntity = event.getEntity();
            if (hurtEntity instanceof Player playerTarget && dmgSourceEntity instanceof Player playerSource) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(playerTarget.blockPosition(), NO_PVP, getDimKey(playerSource), playerSource);
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
                processCheck(checkEvent, onDeny -> {
                    event.setCanceled(true);
                    event.setAmount(0f);
                    sendFlagMsg(onDeny);
                });
            }
        }
    }

    @SubscribeEvent
    public static void onPlayerHurt(LivingHurtEvent event) {
        if (ForgeHandlerUtil.isServerSide(event)) {
            Entity hurtEntity = event.getEntity();
            if (hurtEntity instanceof Player playerTarget) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(playerTarget.blockPosition(), INVINCIBLE, getDimKey(playerTarget), null);
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
                processCheck(checkEvent, onDeny -> {
                    event.setCanceled(true);
                    event.setAmount(0f);
                    sendFlagMsg(onDeny);
                });
            }
        }
    }


    /* TODO: Is this test even necessary anymore?
     *   - There is already a PVP flag for onHurt in place
     * */
    @SubscribeEvent
    public static void onReceiveDmg(LivingDamageEvent event) {
        if (ForgeHandlerUtil.isServerSide(event)) {
            if (event.getSource() == null || event.getEntity() == null) return;
            Entity dmgSourceEntity = event.getSource().getDirectEntity();
            if (dmgSourceEntity instanceof Player dmgSource && event.getEntity() instanceof Player dmgTarget) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(dmgTarget.blockPosition(), MELEE_PLAYERS, getDimKey(dmgSource), dmgSource);
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
                // another check for PVP - this does not prevent knock-back? but prevents dmg
                processCheck(checkEvent, onDeny -> {
                    event.setCanceled(true);
                    event.setAmount(0f);
                    sendFlagMsg(onDeny);
                });
            }
        }
    }

    @SubscribeEvent
    public static void onPlayerKnockback(LivingKnockBackEvent event) {
        if (ForgeHandlerUtil.isServerSide(event)) {
            if (event.getEntity() instanceof Player dmgTarget) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(dmgTarget.blockPosition(), KNOCKBACK_PLAYERS, getDimKey(dmgTarget), null);
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
                processCheck(checkEvent, onDeny -> {
                    event.setCanceled(true);
                    event.setStrength(0);
                    sendFlagMsg(onDeny);
                });
                checkEvent = new FlagCheckEvent(dmgTarget.blockPosition(), INVINCIBLE, getDimKey(dmgTarget), null);
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
                processCheck(checkEvent, onDeny -> {
                    event.setCanceled(true);
                    event.setStrength(0);
                    sendFlagMsg(onDeny);
                });
            }
        }
    }

    @SubscribeEvent
    public static void onPlayerBreakBlock(BlockEvent.BreakEvent event) {
        if (ForgeHandlerUtil.isServerSide(event)) {
            if (event.getPlayer() == null) return;
            Player player = event.getPlayer();
            FlagCheckEvent checkEvent = new FlagCheckEvent(event.getPos(), BREAK_BLOCKS, getDimKey(player), player);
            if (Services.EVENT.post(checkEvent)) {
                return;
            }
            processCheck(checkEvent, onDeny -> {
                event.setCanceled(true);
                updateBlockState((Level) event.getLevel(), event.getPos());
                sendFlagMsg(onDeny);
            });
        }
    }

    @SubscribeEvent
    public static void onPlayerPlaceBlock(BlockEvent.EntityPlaceEvent event) {
        if (ForgeHandlerUtil.isServerSide(event)) {
            if (event.getEntity() == null || !(event.getEntity() instanceof Player player)) {
                return;
            }
            FlagCheckEvent checkEvent = new FlagCheckEvent(event.getPos(), PLACE_BLOCKS, getDimKey(player), player);
            if (Services.EVENT.post(checkEvent)) {
                return;
            }
            processCheck(checkEvent, onDeny -> {
                event.setCanceled(true);
                updateBlockState((Level) event.getLevel(), event.getPos());
                sendFlagMsg(onDeny);
            });
        }
    }

    @SubscribeEvent(priority = EventPriority.HIGHEST)
    public static void onEntityBreak(AttackEntityEvent event) {
        if (ForgeHandlerUtil.isServerSide(event)) {
            if (event.getTarget() == null || event.getEntity() == null) return;
            Entity target = event.getTarget();
            Player player = event.getEntity();
            Set<String> entityTags = Services.FLAG_CONFIG.getCoveredBlockEntityTags();
            boolean isCoveredByTag = entityTags.stream().anyMatch(entityTag -> {
                ResourceLocation tagRl = new ResourceLocation(entityTag);
                return target.getTags().contains(tagRl.getPath());
            });
            Set<String> entities = Services.FLAG_CONFIG.getCoveredBlockEntities();
            boolean isBlockEntityCovered = entities.stream().anyMatch(entity -> {
                ResourceLocation entityRl = new ResourceLocation(entity);
                ResourceLocation targetRl = ForgeRegistries.ENTITY_TYPES.getKey(target.getType());
                return targetRl != null && targetRl.equals(entityRl);
            });
            if (isBlockEntityCovered || isCoveredByTag) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(event.getTarget().blockPosition(), BREAK_BLOCKS, getDimKey(player), player);
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
                processCheck(checkEvent, onDeny -> {
                    event.setCanceled(true);
                    sendFlagMsg(onDeny);
                });
            }
        }
    }

    // TODO: TEST
    @SubscribeEvent
    public static void onExplosionStarted(ExplosionEvent.Start event) {
        if (isServerSide(event.getLevel())) {
            if (event.getExplosion() == null) return;
            Explosion explosion = event.getExplosion();
            BlockPos explosionPos = new BlockPos((int) explosion.getPosition().x, (int) explosion.getPosition().y, (int) explosion.getPosition().z);
            ResourceKey<Level> dim = event.getLevel().dimension();
            if (explosion.getIndirectSourceEntity() == null) {
                // source entity is null, but we still want to cancel the ignition
                FlagCheckEvent checkEvent = new FlagCheckEvent(explosionPos, IGNITE_EXPLOSIVES, dim);
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
                processCheck(checkEvent, onDeny -> {
                    event.setCanceled(true);
                });
            } else {
                if (explosion.getIndirectSourceEntity() instanceof Player player) {
                    FlagCheckEvent checkEvent = new FlagCheckEvent(explosionPos, IGNITE_EXPLOSIVES, dim, player);
                    if (Services.EVENT.post(checkEvent)) {
                        return;
                    }
                    processCheck(checkEvent, onDeny -> {
                        event.setCanceled(true);
                        sendFlagMsg(onDeny);
                    });
                }
                if (explosion.getIndirectSourceEntity() instanceof Monster) {
                    FlagCheckEvent checkEvent = new FlagCheckEvent(explosionPos, MOB_GRIEFING, dim);
                    if (Services.EVENT.post(checkEvent)) {
                        return;
                    }
                    processCheck(checkEvent, onDeny -> {
                        event.setCanceled(true);
                    });
                } else {
                    FlagCheckEvent checkEvent = new FlagCheckEvent(explosionPos, IGNITE_EXPLOSIVES, dim);
                    if (Services.EVENT.post(checkEvent)) {
                        return;
                    }
                    processCheck(checkEvent, onDeny -> {
                        event.setCanceled(true);
                    });
                }
            }
        }
    }

    @SubscribeEvent
    public static void onBonemealUse(BonemealEvent event) {
        if (notServerSideOrPlayerNull(event.getEntity())) return;
        Player player = event.getEntity();
        FlagCheckEvent checkEvent = new FlagCheckEvent(event.getPos(), USE_BONEMEAL, getDimKey(player), player);
        if (Services.EVENT.post(checkEvent)) {
            return;
        }
        processCheck(checkEvent, onDeny -> {
            event.setCanceled(true);
            sendFlagMsg(onDeny);
        });

    }

    @SubscribeEvent
    public static void onPlayerUseEnderPearl(EntityTeleportEvent event) {
        if (ForgeHandlerUtil.isServerSide(event)) {
            // handle player teleportation using ender pearls
            if (event instanceof EntityTeleportEvent.EnderPearl enderPearlEvent) {
                if (enderPearlEvent.getPlayer() == null) return;
                Player player = enderPearlEvent.getPlayer();
                BlockPos target = new BlockPos((int) event.getTarget().x, (int) event.getTarget().y, (int) event.getTarget().z);
                FlagCheckEvent checkEvent = new FlagCheckEvent(target, USE_ENDERPEARL_TO_REGION, getDimKey(player), player);
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
                FlagState flagState = processCheck(checkEvent, onDeny -> {
                    event.setCanceled(true);
                    sendFlagMsg(onDeny);
                });
                if (flagState == FlagState.DENIED) return;

                target = new BlockPos((int) event.getTarget().x, (int) event.getTarget().y, (int) event.getTarget().z);
                checkEvent = new FlagCheckEvent(target, USE_ENDERPEARL_FROM_REGION, getDimKey(player), player);
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
                processCheck(checkEvent, onDeny -> {
                    event.setCanceled(true);
                    sendFlagMsg(onDeny);
                });
                player.getInventory().setChanged();
            }
        }
    }

    @SubscribeEvent
    public static void onPlayerUseToolSecondary(BlockEvent.BlockToolModificationEvent event) {
        if (ForgeHandlerUtil.isServerSide(event)) {
            Player player = event.getPlayer();
            if (player == null) return;
            BlockPos target = event.getPos();
            ResourceKey<Level> dim = getDimKey(player);
            FlagCheckEvent checkEvent = new FlagCheckEvent(target, TOOL_SECONDARY_USE, dim, player);
            if (Services.EVENT.post(checkEvent)) {
                return;
            }
            FlagState flagState = processCheck(checkEvent, onDeny -> {
                event.setCanceled(true);
                sendFlagMsg(onDeny);
            });
            if (flagState == FlagState.DENIED) return;

            if (event.getToolAction().equals(ToolActions.AXE_STRIP)) {
                checkEvent = new FlagCheckEvent(target, AXE_STRIP, dim, player);
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
            }
            if (event.getToolAction().equals(ToolActions.HOE_TILL)) {
                checkEvent = new FlagCheckEvent(target, HOE_TILL, dim, player);
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
            }
            if (event.getToolAction().equals(ToolActions.SHOVEL_FLATTEN)) {
                checkEvent = new FlagCheckEvent(target, SHOVEL_PATH, dim, player);
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
            }
            if (checkEvent != null) {
                processCheck(checkEvent, onDeny -> {
                    event.setCanceled(true);
                    sendFlagMsg(onDeny);
                });
            }
        }
    }

    @SubscribeEvent
    public static void onPlayerRightClickBlock(PlayerInteractEvent.RightClickBlock event) {
        if (notServerSideOrPlayerNull(event.getEntity())) return;
        Player player = event.getEntity();
        UseOnContext useOnContext = new UseOnContext(player, event.getHand(), event.getHitVec());
        BlockPos targetPos = useOnContext.getClickedPos();
        BlockPos placeBlockTarget = targetPos.relative(useOnContext.getClickedFace().getOpposite());
        BlockEntity targetEntity = event.getLevel().getBlockEntity(event.getPos());
        InteractionHand usedHand = useOnContext.getHand();
        boolean hasEmptyHand = hasEmptyHand(player, usedHand);
        ItemStack itemInHand = useOnContext.getItemInHand();
        boolean isSneakingWithEmptyHand = player.isShiftKeyDown() && hasEmptyHand;
        boolean isBlockEntity = targetEntity instanceof BlockEntity;
        boolean isLockableTileEntity = targetEntity instanceof BaseContainerBlockEntity;
        boolean isEnderChest = targetEntity instanceof EnderChestBlockEntity;
        boolean isContainer = targetEntity instanceof LecternBlockEntity || isLockableTileEntity;
        BlockHitResult pos = event.getHitVec();

        // used to allow player to place blocks when shift clicking container on usable bock
        if (isSneakingWithEmptyHand || !player.isShiftKeyDown()) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(pos.getBlockPos(), USE_BLOCKS, getDimKey(player), player);
            if (Services.EVENT.post(checkEvent)) {
                return;
            }
            processCheck(checkEvent, onDeny -> {
                event.setCanceled(true);
                sendFlagMsg(onDeny);
                event.getLevel().updateNeighborsAt(pos.getBlockPos(), event.getLevel().getBlockState(pos.getBlockPos()).getBlock());
            });

            // Note: following flags are already covered with use_blocks
            // check for ender chest access
            if (isEnderChest) {
                checkEvent = new FlagCheckEvent(event.getPos(), ENDER_CHEST_ACCESS, getDimKey(player), player);
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
                processCheck(checkEvent, onDeny -> {
                    event.setCanceled(true);
                    sendFlagMsg(onDeny);
                });
            }
            // check for container access
            if (isContainer) {
                checkEvent = new FlagCheckEvent(event.getPos(), CONTAINER_ACCESS, getDimKey(player), player);
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
                processCheck(checkEvent, onDeny -> {
                    event.setCanceled(true);
                    sendFlagMsg(onDeny);
                });
            }
        }
        if (!hasEmptyHand) {
            ResourceLocation itemRl = ForgeRegistries.ITEMS.getKey(itemInHand.getItem());
            Set<String> entities = Services.FLAG_CONFIG.getCoveredBlockEntities();
            Set<String> entityTags = Services.FLAG_CONFIG.getCoveredBlockEntityTags();
            boolean isCoveredByTag = entityTags.stream().anyMatch(tag -> {
                ResourceLocation tagRl = new ResourceLocation(tag);
                return itemInHand.getTags().anyMatch(itemTagKey -> itemTagKey.location().equals(tagRl));
            });
            boolean isBlockCovered = entities.stream().anyMatch(entity -> {
                ResourceLocation entityRl = new ResourceLocation(entity);
                return itemRl != null && itemRl.equals(entityRl);
            });

            Consumer<FlagCheckResult> onDenyAction = denyResult -> {
                event.setCanceled(true);
                sendFlagMsg(denyResult);
                player.getInventory().setChanged();
            };

            if (isBlockCovered || isCoveredByTag) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(placeBlockTarget, PLACE_BLOCKS, getDimKey(player), player);
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
                processCheck(checkEvent, onDenyAction);
            }

            FlagCheckEvent checkEvent = new FlagCheckEvent(event.getPos(), USE_ITEMS, getDimKey(player), player);
            if (Services.EVENT.post(checkEvent)) {
                return;
            }
            processCheck(checkEvent, onDenyAction);
        }

    }

    @SubscribeEvent
    public static void onAccessEntityContainer(PlayerInteractEvent.EntityInteract event) {
        if (notServerSideOrPlayerNull(event.getEntity())) return;
        Player player = event.getEntity();
        boolean hasInventory = event.getTarget() instanceof Container || event.getTarget() instanceof MenuProvider;
        if (hasInventory) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(event.getTarget().blockPosition(), CONTAINER_ACCESS, getDimKey(player), player);
            if (Services.EVENT.post(checkEvent)) {
                return;
            }
            processCheck(checkEvent, onDeny -> {
                event.setCanceled(true);
                sendFlagMsg(onDeny);
            });
        }
    }

    @SubscribeEvent
    public static void onEntityInteraction(PlayerInteractEvent.EntityInteractSpecific event) {
        if (notServerSideOrPlayerNull(event.getEntity())) return;
        Player player = event.getEntity();
        FlagCheckEvent checkEvent = new FlagCheckEvent(event.getTarget().blockPosition(), USE_ENTITIES, getDimKey(player), player);
        if (Services.EVENT.post(checkEvent)) {
            return;
        }
        processCheck(checkEvent, onDeny -> {
            event.setCanceled(true);
            sendFlagMsg(onDeny);
        });
        if (!hasEmptyHand(player, event.getHand())) {

            checkEvent = new FlagCheckEvent(event.getPos(), USE_ITEMS, getDimKey(player), player);
            if (Services.EVENT.post(checkEvent)) {
                return;
            }
            processCheck(checkEvent, onDeny -> {
                event.setCanceled(true);
                sendFlagMsg(onDeny);
            });
        }

    }

    private static boolean hasEmptyHands(Player player) {
        return player.getItemInHand(InteractionHand.MAIN_HAND).getItem().equals(Items.AIR) && player.getItemInHand(InteractionHand.OFF_HAND).getItem().equals(Items.AIR);
    }

    private static boolean hasEmptyHand(Player player, InteractionHand hand) {
        return player.getItemInHand(hand).getItem().equals(Items.AIR);
    }

    @SubscribeEvent
    public static void onEntityInteraction(PlayerInteractEvent.EntityInteract event) {
        if (notServerSideOrPlayerNull(event.getEntity())) return;
        Player player = event.getEntity();
        FlagCheckEvent checkEvent = new FlagCheckEvent(event.getTarget().blockPosition(), USE_ENTITIES, getDimKey(player), player);
        if (Services.EVENT.post(checkEvent)) {
            return;
        }
        processCheck(checkEvent, onDeny -> {
            event.setCanceled(true);
            sendFlagMsg(onDeny);
        });
        if (!hasEmptyHand(player, event.getHand())) {
            checkEvent = new FlagCheckEvent(event.getPos(), USE_ENTITIES, getDimKey(player), player);
            if (Services.EVENT.post(checkEvent)) {
                return;
            }
            processCheck(checkEvent, onDeny -> {
                event.setCanceled(true);
                sendFlagMsg(onDeny);
            });
        }

    }

    @SubscribeEvent
    public static void onEntityInteraction(PlayerInteractEvent.RightClickItem event) {
        if (notServerSideOrPlayerNull(event.getEntity())) return;
        Player player = event.getEntity();
        FlagCheckEvent checkEvent;

        if (!hasEmptyHand(player, event.getHand())) {
            checkEvent = new FlagCheckEvent(event.getPos(), USE_ENTITIES, getDimKey(player), player);
            if (Services.EVENT.post(checkEvent)) {
                return;
            }
            processCheck(checkEvent, onDeny -> {
                event.setCanceled(true);
                sendFlagMsg(onDeny);
            });
        }
        checkEvent = new FlagCheckEvent(event.getPos(), USE_ITEMS, getDimKey(player), player);
        if (Services.EVENT.post(checkEvent)) {
            return;
        }
        processCheck(checkEvent, onDeny -> {
            event.setCanceled(true);
            sendFlagMsg(onDeny);
        });


    }

    /**
     * Prevents players from using activator blocks like pressure plates
     * TODO: This is very jank implementation. Needs to be tested with multiple players. Move check to activator block itself
     */
    @SubscribeEvent
    public static void onSteppedOnActivator(BlockEvent.NeighborNotifyEvent event) {
        if (ForgeHandlerUtil.isServerSide(event)) {
            Block block = event.getLevel().getBlockState(event.getPos()).getBlock();
            BlockPos pos = event.getPos();
            if (block instanceof BasePressurePlateBlock) {
                AABB areaAbovePressurePlate = new AABB(pos.getX() - 1, pos.getY(), pos.getZ() - 1, pos.getX() + 1, pos.getY() + 2, pos.getZ() + 1);
                List<Player> players = event.getLevel().getEntities(EntityType.PLAYER, areaAbovePressurePlate, (player) -> true);
                final FlagState[] cumulativeState = {FlagState.UNDEFINED};
                Map<Player, FlagCheckEvent> playerCheckEventMap = new HashMap<>();
                for (Player player : players) {
                    FlagCheckEvent checkEvent = new FlagCheckEvent(player.blockPosition(), USE_BLOCKS, getDimKey(player), player);
                    if (Services.EVENT.post(checkEvent)) {
                        return;
                    }
                    playerCheckEventMap.put(player, checkEvent);
                }
                for (Map.Entry<Player, FlagCheckEvent> entry : playerCheckEventMap.entrySet()) {
                    FlagState state = processCheck(entry.getValue(), null, MessageSender::sendFlagMsg);
                    if (state == FlagState.DENIED) {
                        cumulativeState[0] = state;
                    }
                }
                if (cumulativeState[0] == FlagState.DENIED) {
                    event.setCanceled(true);
                }
            }
        }
    }

    /**
     * Note: Does not prevent from fluids generate additional blocks (cobble generator). Use BlockEvent.FluidPlaceBlockEvent for this
     * TODO: Maybe create own mixin like in fabric to better differentiate
     */
    @SubscribeEvent
    public static void onBucketFill(FillBucketEvent event) {
        // Note: FilledBucket seems to always be null. use maxStackSize to determine bucket state (empty or filled)
        if (notServerSideOrPlayerNull(event.getEntity())) return;
        Player player = event.getEntity();

        DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getDimKey(event.getEntity()));
        if (event.getTarget() != null) {
            HitResult pos = event.getTarget();
            BlockPos targetPos = new BlockPos((int) event.getTarget().getLocation().x, (int) event.getTarget().getLocation().y, (int) event.getTarget().getLocation().z);
            // MaxStackSize: 1 -> full bucket so only placeable; >1 -> empty bucket, only fillable
            int bucketItemMaxStackCount = event.getEmptyBucket().getMaxStackSize();
            // placing fluid
            if (bucketItemMaxStackCount == 1) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(targetPos, PLACE_FLUIDS, getDimKey(player), player);
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
                processCheck(checkEvent, onDeny -> {
                    event.setCanceled(true);
                    sendFlagMsg(onDeny);
                });
            }
            // scooping fluid (breaking fluid)
            if (bucketItemMaxStackCount > 1) {
                boolean isWaterlogged = false;
                boolean isFluid = false;
                if (pos != null && pos.getType() == HitResult.Type.BLOCK) {
                    BlockState blockState = event.getLevel().getBlockState(targetPos);
                    // check for waterlogged block
                    if (blockState.getBlock() instanceof SimpleWaterloggedBlock) {
                        isWaterlogged = blockState.getValue(BlockStateProperties.WATERLOGGED);
                    }
                    // check if entityPos has a fluid tag
                    ITagManager<Fluid> tags = ForgeRegistries.FLUIDS.tags();
                    if (tags != null) {
                        isFluid = tags.getTagNames().anyMatch(tag -> blockState.getFluidState().is(tag));
                    }
                    if (isWaterlogged || isFluid) {
                        FlagCheckEvent checkEvent = new FlagCheckEvent(targetPos, SCOOP_FLUIDS, getDimKey(player), player);
                        if (Services.EVENT.post(checkEvent)) {
                            return;
                        }
                        processCheck(checkEvent, onDeny -> {
                            event.setCanceled(true);
                            sendFlagMsg(onDeny);
                        });
                    }
                }
            }
        }

    }

    /**
     * TODO: Flag for team chat
     * Note: message received from server but not distributed to all clients
     */
    @SubscribeEvent
    public static void onSendChat(ServerChatEvent event) {
        if (event.getPlayer() == null) return;
        ServerPlayer player = event.getPlayer();
        FlagCheckEvent checkEvent = new FlagCheckEvent(player.blockPosition(), SEND_MESSAGE, getDimKey(player), player);
        if (Services.EVENT.post(checkEvent)) {
            return;
        }
        processCheck(checkEvent, onDeny -> {
            event.setCanceled(true);
            sendFlagMsg(onDeny);
        });

    }

    @SubscribeEvent
    public static void onCommandSend(CommandEvent event) {
        try {
            Player player = event.getParseResults().getContext().getSource().getPlayerOrException();
            FlagCheckEvent checkEvent = new FlagCheckEvent(player.blockPosition(), EXECUTE_COMMAND, getDimKey(player), player);
            if (Services.EVENT.post(checkEvent)) {
                return;
            }
            processCheck(checkEvent, onDeny -> {
                event.setCanceled(true);
                sendFlagMsg(onDeny);
            });
        } catch (CommandSyntaxException e) {
            // Most likely thrown because command was not send by a player.
            // This is fine because we don't want this flag to be triggered from non-players entities
        }
    }

    @SubscribeEvent
    public static void onPlayerAttemptSleep(SleepingTimeCheckEvent event) {
        if (notServerSideOrPlayerNull(event.getEntity())) return;
        Player player = event.getEntity();
        event.getSleepingLocation().ifPresent((pos) -> {
            FlagCheckEvent checkEvent = new FlagCheckEvent(pos, SLEEP, getDimKey(player), player);
            if (Services.EVENT.post(checkEvent)) {
                return;
            }
            processCheck(checkEvent, onDeny -> {
                event.setResult(Event.Result.DENY);
                sendFlagMsg(onDeny);
            });
        });

    }

    @SubscribeEvent
    public static void onSetSpawn(PlayerSetSpawnEvent event) {
        if (notServerSideOrPlayerNull(event.getEntity())) return;
        BlockPos newSpawn = event.getNewSpawn();
        Player player = event.getEntity();
        if (newSpawn != null) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(newSpawn, SET_SPAWN, getDimKey(player), player);
            if (Services.EVENT.post(checkEvent)) {
                return;
            }
            processCheck(checkEvent, onDeny -> {
                event.setCanceled(true);
                sendFlagMsg(onDeny);
            });
        }
    }

    @SubscribeEvent
    public static void onPlayerDropItem(ItemTossEvent event) {
        if (!event.getPlayer().getCommandSenderWorld().isClientSide) {
            Player player = event.getPlayer();
            if (player == null) return;
            FlagCheckEvent checkEvent = new FlagCheckEvent(event.getEntity().blockPosition(), ITEM_DROP, getDimKey(player), player);
            if (Services.EVENT.post(checkEvent)) {
                return;
            }
            processCheck(checkEvent, onDeny -> {
                event.setCanceled(true);
                player.addItem(event.getEntity().getItem());
                player.getInventory().setChanged();
                sendFlagMsg(onDeny);
            });
        }
    }

    /**
     * Idea: Flags for different animals to mount
     */
    @SubscribeEvent
    public static void onEntityMountAttempt(EntityMountEvent event) {
        if (ForgeHandlerUtil.isServerSide(event)) {
            Entity entityBeingMounted = event.getEntityBeingMounted();
            if (event.getEntityMounting() instanceof Player player) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(entityBeingMounted.blockPosition(), ANIMAL_MOUNTING, getDimKey(player), player);
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
                processCheck(checkEvent, onDeny -> {
                    event.setCanceled(true);
                    sendFlagMsg(onDeny);
                });
                if (event.isDismounting()) {
                    checkEvent = new FlagCheckEvent(entityBeingMounted.blockPosition(), ANIMAL_UNMOUNTING, getDimKey(player), player);
                    if (Services.EVENT.post(checkEvent)) {
                        return;
                    }
                    HandlerUtil.processCheck(checkEvent, onDeny -> {
                        event.setCanceled(true);
                        sendFlagMsg(onDeny);
                    });
                }
            }
        }
    }
}